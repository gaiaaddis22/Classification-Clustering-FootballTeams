Il database relazionale “Football Database” contiene 7 tabelle relative alle principali statistiche (su squadre e
calciatori) dei migliori 5 campionati di calcio europei dal 2014 al 2020.
Fonte database: https://www.kaggle.com/datasets/technika148/football-database
Il dataset oggetto d’analisi stats_summary è stato creato ad hoc gestendo 4 delle 7 tabelle disponibili.
Si rimanda al codice per una descrizione più dettagliata della fase di data engineering.
stats_summary è formato da 686 osservazioni (squadre di calcio dei 5 massimi camionati) e 13 variabili.

L'obiettivo dell'analisi è duplice: da un lato, utilizzando i dati relativi alle prestazioni offensive e difensive della
squadra durante la stagione, si intende determinare in quale fascia della classifica terminerà il campionato (alta,
medio alta, medio bassa, bassa); dall'altro, si desidera individuare dei cluster nascosti interpretando i risultati
ottenuti grazie alle informazioni raccolte.
