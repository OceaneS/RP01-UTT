dataFrance;
param = [1e+20; 1e-07];
verhulstGrowth = @(x, param) param(1)./(1+(param(1)/data(1)-1)*exp(-param(2)*(x-dateSampling(1))));
pkg load optim;
[~, param] = leasqr(dateSampling, data, param, verhulstGrowth, eps, 1000000);
pkg unload optim;
plot(dateSampling, verhulstGrowth(dateSampling, param));