dataFrance;
param = [1e+10; 1e-08];
pkg load optim;
gompertzGrowth = @(x, param) param(1)*exp(log(data(1)/param(1))*exp(-param(2)*(x-dateSampling(1))));
[~, param] = leasqr(dateSampling,data, param, gompertzGrowth, [], 100);
pkg unload optim;
plot(dateSampling, gompertzGrowth(dateSampling, param));