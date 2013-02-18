function ZLsaving(fname, outname)

load(fname)
N = 300;

setsize = data.setsize;

saveme = [data.probedcolor(1:N)' data.binResponse(1:N)' data.setsize(1:N)];
save(outname, 'saveme', '-ascii')

end

