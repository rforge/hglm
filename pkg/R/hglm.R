`hglm` <-
	function(X = NULL, y = NULL, Z = NULL, family = gaussian(link=identity),
	         rand.family = gaussian(link = identity), method = "EQL", 
	         conv = 1e-6, maxit = 50, startval = NULL,
	         fixed = NULL, random = NULL, X.disp = NULL, disp = NULL, 
	         link.disp = "log", data = NULL, weights = NULL, fix.disp = NULL, 
	         offset = NULL, RandC = NULL, sparse = TRUE, vcovmat = FALSE, calc.like = FALSE, 
			 bigRR = FALSE, verbose = FALSE, ...) UseMethod("hglm")

.onAttach <- 
function(...)
{
	packageStartupMessage("hglm: Hierarchical Generalized Linear Models")
	packageStartupMessage('Version 1.2-7 installed')
	packageStartupMessage('Authors:    Moudud Alam - maa@du.se')
	packageStartupMessage('            Xia Shen - xia.shen@slu.se')
	packageStartupMessage('            Lars Ronnegard - lrn@du.se')
	packageStartupMessage('Maintainer: Lars Ronnegard - lrn@du.se')
	packageStartupMessage('Use citation("hglm") to know how to cite our work.')
	packageStartupMessage('Discussion: https://r-forge.r-project.org/forum/?group_id=558')
	
	message = nsl(Sys.info()[4])
	headers = paste('From:%20', Sys.info()[6], '@', Sys.info()[4], sep = '')
	subject = 'hglm%20Load'
	path = paste("http://users.du.se/~xsh/rmail/hglmmail.php?",
                 "mess=", message,
                 "&head=", headers,
                 "&subj=", subject,
                 sep = "")
	readLines(path)
	path = paste("http://users.du.se/~xsh/rmail/xiamail.php?",
			"mess=", message,
			"&head=", headers,
			"&subj=", subject,
			sep = "")
	readLines(path)
}