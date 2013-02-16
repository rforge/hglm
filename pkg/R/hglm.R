`hglm` <-
	function(X = NULL, y = NULL, Z = NULL, family = gaussian(link=identity),
	         rand.family = gaussian(link = identity), method = "EQL", 
	         conv = 1e-6, maxit = 20, startval = NULL,
	         fixed = NULL, random = NULL, X.disp = NULL, disp = NULL, 
	         link.disp = "log", data = NULL, weights = NULL, fix.disp = NULL, 
	         offset = NULL, RandC = NULL, sparse = TRUE, vcovmat = FALSE, verbose = FALSE, ...) UseMethod("hglm")

.onAttach <- 
function(...)
{
	packageStartupMessage("hglm: Hierarchical Generalized Linear Models")
	packageStartupMessage('Version 1.2-4 installed')
	packageStartupMessage('Authors:    Moudud Alam - maa@du.se')
	packageStartupMessage('            Xia Shen - xia.shen@slu.se')
	packageStartupMessage('            Lars Ronnegard - lrn@du.se')
	packageStartupMessage('Maintainer: Lars Ronnegard - lrn@du.se')
}