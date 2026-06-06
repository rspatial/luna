# List and Download Landsat products
# Authors: Alex Mandel, Aniruddha Ghosh, Robert J. Hijmans
# Originally written 2019; rewritten 2026 to use the USGS Landsat STAC
# (https://landsatlook.usgs.gov/stac-server/) because NASA CMR no longer
# indexes USGS Landsat granules and the public AWS Collection-1 bucket
# was retired alongside Collection 1 on 2022-12-30.

.landsat_stac_url <- "https://landsatlook.usgs.gov/stac-server"

# Map legacy or shorthand product names onto current STAC collection ids.
# The collections we support today are the three "Level" products that cover
# the standard Landsat 4-9 archive. Anything else is passed through.
.landsat_translate_product <- function(product) {
	tab <- list(
		"landsat-c2l1"    = c("landsat-c2l1", "L1", "level-1",
		                     "Landsat_8_OLI_TIRS_C1", "Landsat_8_OLI_TIRS_C2",
		                     "Landsat7_ETM_Plus_C1", "Landsat7_ETM_Plus_C2_L1",
		                     "Landsat4-5_TM_C1",     "Landsat4-5_TM_C2_L1",
		                     "Landsat1-5_MSS_C1",    "Landsat1-5_MSS_C2_L1",
		                     "Landsat8-9_OLI_TIRS_C2_L1"),
		"landsat-c2l2-sr" = c("landsat-c2l2-sr", "SR", "L2SR",
		                     "LANDSAT_SURFACE_REFLECTANCE_L8_OLI_TIRS",
		                     "LANDSAT_SURFACE_REFLECTANCE_L7_ETM",
		                     "LANDSAT_SURFACE_REFLECTANCE_L4-5_TM"),
		"landsat-c2l2-st" = c("landsat-c2l2-st", "ST", "L2ST")
	)
	for (cid in names(tab)) {
		if (product %in% tab[[cid]]) {
			if (product != cid) {
				message(sprintf("translating product '%s' -> STAC collection '%s'", product, cid))
			}
			return(cid)
		}
	}
	product
}


# Issue a STAC /search query against the USGS Landsat STAC catalog.
# Returns a list of features (each feature is a list with $assets, etc.) or
# an empty list if no matches.
.landsat_stac_search <- function(product, start_date, end_date, extent,
                                 limit = 1000, ...) {
	e <- .getExtent(extent)
  # .getExtent returns "minlon,minlat,maxlon,maxlat" (CMR ordering).
  # STAC bbox is [minlon, minlat, maxlon, maxlat].
	bbox <- as.numeric(strsplit(e, ",")[[1]])
	if (length(bbox) != 4) stop("could not parse aoi/extent into a 4-element bbox")

	start_date <- as.Date(start_date)
	end_date   <- as.Date(end_date)
	datetime   <- paste0(start_date, "T00:00:00Z/", end_date, "T23:59:59Z")

	body <- list(
		collections = list(product),
		bbox        = as.list(bbox),
		datetime    = datetime,
		limit       = min(limit, 1000)
	)

	out <- list()
	url <- file.path(.landsat_stac_url, "search")
	repeat {
		r <- httr::POST(url,
		                body = jsonlite::toJSON(body, auto_unbox = TRUE),
		                httr::content_type_json())
		httr::stop_for_status(r)
		j <- jsonlite::fromJSON(httr::content(r, as = "text"), simplifyVector = FALSE)
		if (length(j$features) == 0) break
		out <- c(out, j$features)
		if (length(out) >= limit) {
			out <- out[seq_len(limit)]
			break
		}
		nxt <- vapply(j$links, function(l) isTRUE(l$rel == "next"), logical(1))
		if (!any(nxt)) break
		nxt_link <- j$links[[which(nxt)[1]]]
		body <- nxt_link$body
		url  <- nxt_link$href
	}
	out
}


# Build a data.frame describing the assets we want to download. One row per
# (scene, asset_key). If `bands` is NULL we return everything; otherwise we
# filter by asset key (case insensitive).
.landsat_assets <- function(features, bands = NULL) {
	rows <- vector("list", length(features))
	for (i in seq_along(features)) {
		f <- features[[i]]
		if (length(f$assets) == 0) next
		keys  <- names(f$assets)
		hrefs <- vapply(f$assets, function(a) a$href %||% NA_character_, character(1))
		keep  <- !is.na(hrefs) & nzchar(hrefs)
		if (!is.null(bands)) {
			keep <- keep & (tolower(keys) %in% tolower(bands))
		}
		if (!any(keep)) next
		rows[[i]] <- data.frame(
			scene  = f$id,
			asset  = keys[keep],
			href   = hrefs[keep],
			stringsAsFactors = FALSE
		)
	}
	rows <- rows[!vapply(rows, is.null, logical(1))]
	if (length(rows) == 0) {
		return(data.frame(scene = character(), asset = character(),
		                  href = character(), stringsAsFactors = FALSE))
	}
	do.call(rbind, rows)
}


# Establish an authenticated session against ers.cr.usgs.gov by submitting
# the HTML login form. Returns a curl handle on success or stops with an
# informative error.
.ers_login <- function(username, password, verbose = TRUE) {
	hh <- httr::handle("https://ers.cr.usgs.gov")
	r <- httr::GET("https://ers.cr.usgs.gov/login", handle = hh)
	httr::stop_for_status(r, "fetch the EROS login page")

	html <- httr::content(r, as = "parsed")
	hids <- xml2::xml_find_all(html, "//form//input[@type='hidden']")
	hkv  <- stats::setNames(xml2::xml_attr(hids, "value"),
	                        xml2::xml_attr(hids, "name"))
	if (!"csrf" %in% names(hkv)) {
		stop("could not find csrf token on the EROS login page; the EROS UI may have changed.\n",
		     "Visit https://earthexplorer.usgs.gov/ to download Landsat manually, or use the `rstac` package.",
		     call. = FALSE)
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
		stop("EROS login failed (HTTP ", httr::status_code(pr), "). ",
		     "Check your USGS Earthdata Login `username` and `password`, ",
		     "and that you have completed the EROS account setup at ",
		     "https://ers.cr.usgs.gov/. ",
		     "If logging in via a browser works but this still fails, the EROS UI may have changed; ",
		     "in that case use the `rstac` package or download manually from earthexplorer.usgs.gov.",
		     call. = FALSE)
	}
	hh
}


# Download a single asset URL through an authenticated EROS handle.
# Validates the response so an HTML login/error page is never silently saved.
.ers_download_one <- function(url, path, handle, overwrite = FALSE, verbose = TRUE) {
	outfile <- .cmr_outfile(url, path)

	if (file.exists(outfile) && !overwrite) {
		fsz <- file.info(outfile)$size
		if (isTRUE(fsz < 1024)) {
			file.remove(outfile)
		} else {
			return(outfile)
		}
	}

	args <- list(url, handle = handle,
	             httr::config(followlocation = TRUE, ssl_verifypeer = 0),
	             httr::write_disk(outfile, overwrite = TRUE))
	if (verbose) args <- c(args, list(httr::progress()))
	f <- do.call(httr::GET, args)

	err <- .cmr_validate_response(f, outfile)
	if (!is.null(err)) {
		if (file.exists(outfile)) file.remove(outfile)
		stop(err)
	}
	outfile
}


getLandsat <- function(product = "landsat-c2l1", start_date, end_date, aoi,
                       download = FALSE, path,
                       username, password,
                       bands = NULL,
                       limit = 1000, overwrite = FALSE,
                       verbose = TRUE, ...) {

	if (missing(start_date)) stop("provide a start_date")
	if (missing(end_date))   stop("provide an end_date")
	if (missing(aoi))        stop("provide an area of interest")

  # Politely accept (and ignore) legacy arguments so existing scripts do not
  # break with hard errors. NASA CMR no longer indexes Landsat granules, so
  # the old "AWS" / "EROS" `server` distinction is moot - everything goes
  # through the USGS STAC catalog now.
	dots <- list(...)
	if (!is.null(dots$server)) {
		warning("`server` is no longer used; all Landsat search now goes through ",
		        "the USGS STAC catalog at ", .landsat_stac_url, call. = FALSE)
	}
	if (!is.null(dots$version)) {
		warning("`version` is no longer used; specify the desired STAC collection ",
		        "via `product` (e.g. 'landsat-c2l1', 'landsat-c2l2-sr', 'landsat-c2l2-st')",
		        call. = FALSE)
	}

	product <- .landsat_translate_product(product)

	feats <- .landsat_stac_search(product    = product,
	                              start_date = start_date,
	                              end_date   = end_date,
	                              extent     = aoi,
	                              limit      = limit)
	if (length(feats) == 0) {
		warning("no Landsat scenes found for product='", product,
		        "', dates ", start_date, "..", end_date, ", and the given AOI",
		        call. = FALSE)
		return(character(0))
	}

	asset_df <- .landsat_assets(feats, bands = bands)
	if (nrow(asset_df) == 0) {
		warning("found ", length(feats), " scenes but no assets matched ",
		        "`bands`=", paste(bands, collapse = ","), call. = FALSE)
		return(character(0))
	}

	if (!download) {
		return(asset_df$href)
	}

	path <- .getPath(path)
	if (missing(username)) stop("provide a USGS Earthdata Login `username`")
	if (missing(password)) stop("provide a USGS Earthdata Login `password`")

	hh <- .ers_login(username, password, verbose = verbose)
	on.exit(try(httr::GET("https://ers.cr.usgs.gov/logout", handle = hh), silent = TRUE),
	        add = TRUE)

	files <- character(nrow(asset_df))
	for (i in seq_len(nrow(asset_df))) {
		if (verbose) {
			message(sprintf("[%d/%d] %s : %s", i, nrow(asset_df),
			                asset_df$scene[i], asset_df$asset[i]))
		}
		f <- tryCatch(
			.ers_download_one(asset_df$href[i], path, hh,
			                  overwrite = overwrite, verbose = verbose),
			error = function(e) e
		)
		if (inherits(f, "error")) {
			warning("failure: ", asset_df$href[i], ": ", conditionMessage(f),
			        call. = FALSE)
		} else {
			files[i] <- f
		}
	}
	files[nzchar(files)]
}


`%||%` <- function(a, b) if (is.null(a)) b else a
