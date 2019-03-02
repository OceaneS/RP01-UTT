dataFrance;
param = 0.000015;
pkg load optim;
malthusGrowth = @(x, p) data(1)*exp(p*(x-dateSampling(1)));
[~,param] = leasqr(dateSampling, data, param, malthusGrowth);
pkg unload optim;
plot(dateSampling, malthusGrowth(dateSampling, param));