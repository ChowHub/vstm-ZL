for (ii in dir()[1:20]){
 	if (substr(ii,1,5)=="VSTMz" && substr(ii, nchar(ii)-2, nchar(ii)) != 'mat') {
 		print(ii)
 		data = read.table(ii)
 		names(data) = vnames
 		data = calcDiff(data)
 		L[strsplit(ii, '_')[[1]][3]] = subset(data, data$SetSize == numstims)
}
}
