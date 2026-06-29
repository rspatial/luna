# Earthdata Login (NASA URS) and USGS EROS authentication
# Author: Robert J. Hijmans (June 2026)
# Licence GPL v3
#
# `earthdataLogin()` and `erosLogin()` are the public entry points. Each
# returns a session object - an environment - that downstream luna
# functions accept via their `auth` argument.
#
# Internal helpers `.new_earthdata_session()` / `.new_eros_session()` build
# a session WITHOUT a credential probe. They power the deprecated
# `username` / `password` path of getNASA() and getLandsat() so existing
# scripts keep working unchanged (with a deprecation warning).
#
# Temp files are removed by reg.finalizer when the session is garbage-
# collected or when R exits.


# ---- NASA Earthdata Login (URS) ------------------------------------------

.new_earthdata_session <- function(username, password, validated = FALSE) {
	netrc_file  <- tempfile("luna_netrc_",  fileext = ".txt")
	cookie_file <- tempfile("luna_cookies_", fileext = ".txt")
	writeLines(
		paste("machine urs.earthdata.nasa.gov login", username, "password", password),
		netrc_file
	)
	file.create(cookie_file)

	s <- new.env(parent = emptyenv())
	s$service     <- "earthdata"
	s$username    <- username
	s$netrc_file  <- netrc_file
	s$cookie_file <- cookie_file
	s$validated   <- isTRUE(validated)
	s$created     <- Sys.time()
	class(s) <- c("earthdata_session", "luna_session")

	reg.finalizer(s, function(e) {
		try(suppressWarnings(file.remove(e$netrc_file)),  silent = TRUE)
		try(suppressWarnings(file.remove(e$cookie_file)), silent = TRUE)
	}, onexit = TRUE)

	s
}


earthdataLogin <- function(username, password, verbose = TRUE) {
	if (missing(username) || !is.character(username) || length(username) != 1 || !nzchar(username)) {
		stop("`username` must be a non-empty character string", call. = FALSE)
	}
	if (missing(password) || !is.character(password) || length(password) != 1 || !nzchar(password)) {
		stop("`password` must be a non-empty character string", call. = FALSE)
	}

	if (verbose) message("Validating Earthdata credentials for user '", username, "' ...")

	probe_url <- "https://urs.earthdata.nasa.gov/api/users/tokens"
	r <- tryCatch(
		httr::GET(probe_url,
		          httr::authenticate(username, password, type = "basic"),
		          httr::timeout(30)),
		error = function(e) e
	)
	if (inherits(r, "error")) {
		stop(.cmr_auth_error(sprintf(
			"could not reach Earthdata Login (urs.earthdata.nasa.gov): %s",
			conditionMessage(r))))
	}

	sc <- httr::status_code(r)
	if (sc == 401 || sc == 403) {
		stop(.cmr_auth_error(sprintf(
			"Earthdata Login rejected the credentials for user '%s' (HTTP %d). Check your username and password at https://urs.earthdata.nasa.gov/, and that you have accepted the relevant data product EULAs.",
			username, sc), status = sc))
	}
	if (sc != 200) {
		stop(.cmr_auth_error(sprintf(
			"unexpected HTTP %d from %s while validating Earthdata credentials",
			sc, probe_url), status = sc))
	}

	s <- .new_earthdata_session(username, password, validated = TRUE)
	if (verbose) message("Earthdata credentials are valid.")
	s
}


# ---- USGS EROS form-login -------------------------------------------------

# The EROS auth flow scrapes the login page for the CSRF token, POSTs the
# login form, and confirms the response carries a "Sign Out" button. The
# resulting curl handle holds the session cookies needed to download
# Landsat assets from landsatlook.usgs.gov.

.ers_form_login <- function(username, password) {
	hh <- httr::handle("https://ers.cr.usgs.gov")
	r <- httr::GET("https://ers.cr.usgs.gov/login", handle = hh)
	httr::stop_for_status(r, "fetch the EROS login page")

	html <- httr::content(r, as = "parsed")
	hids <- xml2::xml_find_all(html, "//form//input[@type='hidden']")
	hkv  <- stats::setNames(xml2::xml_attr(hids, "value"),
	                        xml2::xml_attr(hids, "name"))
	if (!"csrf" %in% names(hkv)) {
		stop(.cmr_auth_error(
			"could not find csrf token on the EROS login page; the EROS UI may have changed. Visit https://earthexplorer.usgs.gov/ to download Landsat manually, or use the `rstac` package."))
	}

	pr <- httr::POST("https://ers.cr.usgs.gov/login",
	                 handle = hh,
	                 httr::add_headers(
	                   Referer = "https://ers.cr.usgs.gov/login",
	                   Origin  = "https://ers.cr.usgs.gov"
	                 ),
	                 body   = c(list(username = username, password = password),
	                            as.list(hkv)),
	                 encode = "form")

	body <- httr::content(pr, as = "text", encoding = "UTF-8")
	signed_in <- httr::status_code(pr) == 200 &&
	             grepl("Sign Out|sign-out", body, ignore.case = TRUE)
	if (!signed_in) {
		stop(.cmr_auth_error(sprintf(
			"EROS login failed (HTTP %d). Check your USGS Earthdata Login `username` and `password`, and that you have completed the EROS account setup at https://ers.cr.usgs.gov/. If logging in via a browser works but this still fails, the EROS UI may have changed; in that case use the `rstac` package or download manually from earthexplorer.usgs.gov.",
			httr::status_code(pr)), status = httr::status_code(pr)))
	}
	hh
}


.new_eros_session <- function(username, password, validated = FALSE) {
	s <- new.env(parent = emptyenv())
	s$service   <- "eros"
	s$username  <- username
	s$password  <- password   # kept so we can lazy-login on first download
	s$handle    <- NULL
	s$validated <- isTRUE(validated)
	s$created   <- Sys.time()
	class(s) <- c("eros_session", "luna_session")

	reg.finalizer(s, function(e) {
		if (!is.null(e$handle)) {
			try(httr::GET("https://ers.cr.usgs.gov/logout", handle = e$handle),
			    silent = TRUE)
		}
	}, onexit = TRUE)

	s
}


# Make sure the eros_session has a live curl handle. If not, log in now.
.eros_ensure_handle <- function(session, verbose = TRUE) {
	if (is.null(session$handle)) {
		if (verbose) message("Logging in to EROS as '", session$username, "' ...")
		session$handle    <- .ers_form_login(session$username, session$password)
		session$validated <- TRUE
	}
	session
}


erosLogin <- function(username, password, verbose = TRUE) {
	if (missing(username) || !is.character(username) || length(username) != 1 || !nzchar(username)) {
		stop("`username` must be a non-empty character string", call. = FALSE)
	}
	if (missing(password) || !is.character(password) || length(password) != 1 || !nzchar(password)) {
		stop("`password` must be a non-empty character string", call. = FALSE)
	}

	if (verbose) message("Validating EROS credentials for user '", username, "' ...")
	hh <- .ers_form_login(username, password)
	s <- .new_eros_session(username, password, validated = TRUE)
	s$handle <- hh
	if (verbose) message("EROS credentials are valid.")
	s
}


# ---- print methods --------------------------------------------------------

print.earthdata_session <- function(x, ...) {
	cat("<earthdata_session> (NASA URS)\n")
	cat("  username : ", x$username,            "\n", sep = "")
	cat("  created  : ", format(x$created),     "\n", sep = "")
	cat("  validated: ", isTRUE(x$validated),   "\n", sep = "")
	invisible(x)
}

print.eros_session <- function(x, ...) {
	cat("<eros_session> (USGS EROS)\n")
	cat("  username : ", x$username,                         "\n", sep = "")
	cat("  created  : ", format(x$created),                  "\n", sep = "")
	cat("  validated: ", isTRUE(x$validated),                "\n", sep = "")
	cat("  handle   : ", if (is.null(x$handle)) "<not yet logged in>" else "<live>",
	    "\n", sep = "")
	invisible(x)
}


# ---- shared helpers -------------------------------------------------------

# Resolve the auth situation for getNASA() / getLandsat(). Returns a session
# of the requested service, emitting a deprecation warning when the legacy
# username/password path is used.
.resolve_auth <- function(auth, username, password, service,
                          username_missing, password_missing) {
	stopifnot(service %in% c("earthdata", "eros"))
	expected_class <- if (service == "earthdata") "earthdata_session" else "eros_session"
	new_fun        <- if (service == "earthdata") .new_earthdata_session else .new_eros_session
	login_call     <- if (service == "earthdata") "earthdataLogin()" else "erosLogin()"

	if (!is.null(auth)) {
		if (!inherits(auth, expected_class)) {
			stop("`auth` must be an `", expected_class, "` returned by `",
			     login_call, "`", call. = FALSE)
		}
		return(auth)
	}

	if (username_missing || password_missing) {
		stop("provide an `auth` session created by `", login_call,
		     "`, or `username` and `password` (deprecated)", call. = FALSE)
	}

	warning(
		"passing `username` and `password` directly is deprecated; ",
		"call `auth <- ", login_call, "` once and pass `auth = auth` instead. ",
		"This may become an error in a future release.",
		call. = FALSE
	)
	new_fun(username, password, validated = FALSE)
}
