`plot.hglm` <-
function(Object, pch = '+', col.theme = 'colorful', output = 'screen',
filename = 'Rplot', ...) {
	hatvalues <- Object$hv
	deviances <- Object$dev
	if (col.theme == 'colorful') {
		pcol <- 4
		lcol <- 2
	}
	else {
		if (col.theme == 'blackwhite') {
			pcol <- lcol <- 1
		}
		else {
			stop('Incorrect color theme is specified!')
		}
	}
	for (i in 1:2) {
		if (output == 'screen') {
			if (i > 1) {
				trial <- try(windows(), silent = TRUE)
				if (inherits(trial, 'try-error')) {
					quartz()
				}
			}
		}
		else {
			if (output == 'postscript') {
				postscript(paste(filename, i, '.ps', sep = ''))
			}
			else {
				if (output == 'pdf') {
					pdf(paste(filename, i, '.pdf', sep = ''))
				}
				else {
					stop('Incorrect output option is specified!')
				}
			}
		}
		if (i == 1) {
			par(mfrow = c(1,2), pty = 's')
			plot(deviances, pch = pch, col = pcol)
			beta <- var(deviances)/mean(deviances)
			alpha <- mean(deviances)/beta
			qqplot(rgamma(9999, alpha, 1/beta), deviances, col = pcol, 
				pch = pch, xlab = 'Gamma Order Statistics')
			abline(0, 1, col = lcol)
		}
		else {
			if (i == 2) {
				plot(hatvalues, pch = pch, col = pcol)
			}
		}
		if (output != 'screen') {
			dev.off()
		}
	}
}

