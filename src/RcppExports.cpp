// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// nnls_solver
arma::mat nnls_solver(arma::mat x, arma::mat A, int iterate, float tolerance);
RcppExport SEXP _luna_nnls_solver(SEXP xSEXP, SEXP ASEXP, SEXP iterateSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< int >::type iterate(iterateSEXP);
    Rcpp::traits::input_parameter< float >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(nnls_solver(x, A, iterate, tolerance));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_luna_nnls_solver", (DL_FUNC) &_luna_nnls_solver, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_luna(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}