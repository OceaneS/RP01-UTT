1;

%Fonction prenant en entrée le vecteur years et la matrice de paramètres du modèle (à partir de laquelle on génère la matrice du modèle). La fonction doit renvoyer data si param est optimisé.
function data = leslieGrowth(years, param)
    % A = [param(1:3,1)'; diag(param(4:5,1)) zeros(2, 1)]'; %On génère la matrice du modèle
    A = [param(1) param(2) param(3)
        param(4) param(6) 0
        0 param(5) param(7)]';
    n0 = [12013879 21072267 5296854]; %On met n(0) = data pour la première année
    for i = 1:length(years)
        data(i,:) = n0*A^(years(i) - years(1)); %On crée chaque ligne de la matrice data en utilisant la suite géométrique matricielle
    endfor
endfunction

% Source : INSEE
% https://www.insee.fr/fr/statistiques/1906664?sommaire=1906743

data = [1920,31.3,54.9,13.8,2.8,38383.0; 1930,30.1,55.7,14.2,2.9,40912.1; 1946,29.5,54.5,16.0,3.4,40125.2; 1950,30.1,53.6,16.2,3.8,41647.3; 1960,32.3,51.0,16.7,4.3,45464.8; 1970,33.1,48.8,18.0,4.7,50528.2; 1980,30.6,52.4,17.0,5.7,53731.4; 1990,27.8,53.2,19.0,6.8,56577.0; 2000,25.6,53.8,20.6,7.2,58858.2; 2010,24.5,52.6,22.8,8.9,62765.2; 2011,24.5,52.2,23.3,9.0,63070.3; 2012,24.4,51.9,23.7,9.1,63376.0; 2013,24.4,51.6,24.1,9.1,63697.9]; %Données extraites du site de l'INSEE
years = data(:,1); %On récupère les années dans la première colonne
data(:,1) = []; %On supprime la première colonne car elle contient les années maintenant dans years
data(:,4) = []; %On retire la 4eme colonne car elle contient des personnes déjà présentes dans la 3eme
data(:,4) = 1000*data(:,4); %On multiplie la 4eme colonne par 1000 car elle est en milliers
for i = 1:size(data)(1) %Petite astuce spécifique à Octave : on utilise size pour obtenir la taille (deux nombres) et on met derrière un indice pour récupérer le premier nombre
    data(i,1:3) = 0.01*data(i,4)* data(i,1:3); %On convertit pour chaque ligne les pourcentages en nombre de personne
endfor
data(:,4) = []; %On retire la dernière colonne car elle ne nous sert plus
param = [1 1 1 1 1 1 1]';
pkg load optim;
convergence = 0;
options.bounds = [0 Inf; 0 Inf; 0 Inf; 0 1; 0 1; 0 1; 0 1];
while (!convergence)
    [result, param, convergence] = leasqr(years, data, param, @leslieGrowth, eps, 100, [], [], [], options);
endwhile
pkg unload optim;
plot(years, data,'b');
hold on;
plot(years, result, 'r');
% plot(years(1):years(end), leslieGrowth(years(1):years(end), param), 'r');
axis tight;