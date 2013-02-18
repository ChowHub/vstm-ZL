path =  'data/';
fnames = what(path);
f = fnames.mat;
for ii=1:length(f),
[pathstr, name, ext] = fileparts(char(f(ii)));
ZLsaving(strcat(path, char(f(ii))), strcat(path, name));
end
