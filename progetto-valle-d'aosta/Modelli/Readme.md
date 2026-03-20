# Modello a blocchi con fixed effects e random effects

## Obiettivo

Questo progetto ha l’obiettivo di modellare il **consumo energetico comunale** e valutare il contributo progressivo di diversi gruppi di variabili, con particolare attenzione all’**impatto del turismo**.

L’idea è costruire un modello interpretabile che permetta di capire:

- quanto il meteo spiega il consumo
- quanto aggiungono le variabili di calendario
- quanto conta la dinamica temporale del consumo
- quanto il turismo migliora la capacità esplicativa del modello

---

## Scelta metodologica

Per questa analisi si adotta un **modello lineare a effetti misti** (*Linear Mixed Effects Model*), con:

- **fixed effects** per stimare l’effetto medio delle variabili osservate
- **random effects** per catturare l’eterogeneità strutturale tra comuni

Questa scelta è particolarmente adatta a dati panel o longitudinali, in cui si osservano più comuni nel tempo.

---

## Forma generale del modello

Il modello può essere scritto come:

$$
\log(Consumo_{it}+1) = \beta_0 + \beta X_{it} + u_i + \varepsilon_{it}
$$

dove:

- \(i\) identifica il comune
- \(t\) identifica il tempo
- \(X_{it}\) rappresenta l’insieme delle variabili esplicative
- \(u_i\) è il **random intercept** del comune
- \(\varepsilon_{it}\) è il termine di errore

La trasformazione logaritmica del consumo aiuta a rendere la distribuzione più stabile e a facilitare l’interpretazione dei coefficienti.
In parole semplici, con il coefficiente u_i = “quanto il comune i è strutturalmente sopra o sotto la media”

---

## Perché una strategia a blocchi

La modellazione segue una **strategia a blocchi**, cioè i gruppi di variabili vengono aggiunti progressivamente.

Questo approccio permette di:

1. isolare il contributo di ogni gruppo di variabili
2. controllare meglio i fattori confondenti
3. misurare l’effetto incrementale del turismo
4. rendere il modello più interpretabile
5. confrontare specificazioni annidate in modo rigoroso

In altre parole, non si inseriscono tutte le variabili insieme fin dall’inizio, ma si costruisce il modello passo dopo passo.

---

## Struttura dei blocchi

### Modello 0 – Baseline

Il primo modello contiene solo l’intercetta e il random effect del comune.  
Serve come riferimento minimo per valutare il miglioramento dei modelli successivi.

```text
log_consumo ~ 1 + (1 | comune)
```

---

### Modello 1 – Blocco Meteo

Nel primo blocco vengono introdotte le variabili ambientali, che rappresentano uno dei driver principali del consumo energetico.

Variabili tipiche:

- temperatura
- precipitazioni
- neve
- HDD
- CDD

```text
log_consumo ~ temperatura + precipitazioni + neve + HDD + CDD +
              (1 | comune)
```

# Commento di Pietro
Quanto il meteo spiega le variazioni del consumo all’interno dei comuni,
tenendo conto che ogni comune ha un livello medio diverso. In sostanza, assumiamo che l'impatto delle variabilie ambientali sia simile da comune a comune (cioe' i coefficienti non cambiano).


---

### Modello 2 – Blocco Calendario

Nel secondo blocco vengono aggiunte le variabili temporali e di calendario, utili a catturare la struttura settimanale e stagionale del consumo.

Variabili tipiche:

- weekend
- festivo
- mese

```text
log_consumo ~ temperatura + precipitazioni + neve + HDD + CDD +
              weekend + festivo + factor(mese) +
              (1 | comune)
```

# Commento Pietro

Qui ci chiediamo due cose prima se l'inserimento degli effetti calendario aumenta la spiegazione del consumo energetico, secondo di quanto.


---

### Modello 3 – Blocco Dinamica temporale

Nel terzo blocco si introduce la dipendenza del consumo dal proprio passato, tramite variabili lag.

Variabili tipiche:

- lag_1 del consumo
- eventuali medie mobili

```text
log_consumo ~ temperatura + precipitazioni + neve + HDD + CDD +
              weekend + festivo + factor(mese) +
              lag_1 +
              (1 | comune)
```


# Commento Pietro

A questo punto dovrebbe essere pleonastico quello che stiamo facendo!


---

### Modello 4 – Blocco Turismo

Nel quarto blocco vengono aggiunte le variabili turistiche, con l’obiettivo di quantificare il loro contributo aggiuntivo rispetto a una baseline già robusta.

Variabili tipiche:

- presenze turistiche
- arrivi turistici
- occupazione alberghiera

```text
log_consumo ~ temperatura + precipitazioni + neve + HDD + CDD +
              weekend + festivo + factor(mese) +
              lag_1 +
              presenze_turistiche + arrivi_turistici + occupazione_alberghiera +
              (1 | comune)
```

---

## Interpretazione dei fixed effects

I **fixed effects** rappresentano l’effetto medio delle variabili osservate sul consumo energetico.

Per esempio:

- un coefficiente positivo della temperatura può indicare maggiore uso di raffrescamento in estate
- un coefficiente positivo di HDD può indicare un aumento del fabbisogno di riscaldamento
- un coefficiente positivo delle presenze turistiche può suggerire che il turismo contribuisce ad aumentare il consumo energetico

L’interpretazione va sempre fatta tenendo costanti le altre variabili presenti nel modello.

---

## Interpretazione dei random effects

I **random effects** permettono di catturare differenze sistematiche tra comuni che non sono spiegate direttamente dalle variabili osservate.

Questo è utile perché due comuni possono avere livelli medi di consumo diversi per ragioni strutturali come:

- dimensione
- composizione del tessuto economico
- caratteristiche urbanistiche
- intensità turistica di fondo
- infrastrutture

Con un **random intercept per comune**, ogni comune può avere un proprio livello medio di consumo.

---

## Perché non inserire tutto subito nello stesso modello

Inserire tutte le variabili in un solo passaggio rende più difficile capire:

- quale blocco stia davvero migliorando il modello
- se il turismo aggiunge informazione propria o se sta solo assorbendo effetti stagionali
- quanto le variabili siano tra loro collineari

La strategia a blocchi risolve questo problema, perché consente di confrontare modelli annidati e di valutare il miglioramento progressivo della specificazione.

---

## Come si valuta il contributo di ogni blocco

I modelli vengono confrontati progressivamente usando metriche come:

- **AIC**
- **BIC**
- **log-likelihood**
- **R² marginale**
- **R² condizionale**

L’attenzione principale è sul confronto tra:

- modello senza turismo
- modello con turismo

Se il modello con turismo migliora in modo consistente, si può concludere che le variabili turistiche forniscono capacità esplicativa addizionale.

---

## Logica dell’analisi

La logica complessiva è la seguente:

1. costruire una baseline minima
2. aggiungere il meteo
3. aggiungere calendario e stagionalità
4. aggiungere la dinamica del consumo
5. aggiungere il turismo
6. confrontare i risultati

Questo consente di rispondere a una domanda chiara:

> Il turismo spiega una parte del consumo energetico che non era già spiegata da meteo, calendario e dinamica temporale?

---

## Variabili minime richieste

Il dataset dovrebbe contenere almeno le seguenti colonne:

| Variabile | Descrizione |
|---|---|
| comune | identificativo del comune |
| data | data dell’osservazione |
| consumo | consumo energetico osservato |
| temperatura | temperatura media |
| precipitazioni | precipitazioni |
| neve | livello o indicatore di neve |
| HDD | heating degree days |
| CDD | cooling degree days |
| weekend | indicatore weekend |
| festivo | indicatore festivo |
| mese | mese di osservazione |
| lag_1 | consumo ritardato di un periodo |
| presenze_turistiche | numero di presenze |
| arrivi_turistici | numero di arrivi |
| occupazione_alberghiera | tasso di occupazione delle strutture |

---

## Possibili estensioni

Il modello può essere esteso in vari modi.

### Random slope per il turismo

Oltre al random intercept, si può consentire all’effetto del turismo di cambiare da comune a comune.

Esempio concettuale:

```text
(1 + presenze_turistiche | comune)
```

Questo permette di modellare il fatto che il turismo possa avere un impatto molto forte in alcuni comuni e più debole in altri.

### Modelli separati per tipologia di utenza

L’analisi può essere replicata separatamente per:

- residenziale
- commerciale
- industriale

In questo modo si può verificare se il turismo impatta soprattutto alcune categorie di consumo.

### Effetti non lineari

Alcune variabili, in particolare la temperatura, potrebbero avere effetti non lineari.  
In questi casi si possono valutare spline, termini quadratici o specificazioni alternative.

---

## Vantaggi dell’approccio scelto

Questo approccio consente di ottenere un modello:

- interpretabile
- progressivo
- robusto
- coerente con la struttura panel dei dati
- adatto a misurare il contributo incrementale del turismo

Non serve solo a prevedere il consumo, ma soprattutto a **capire come ogni gruppo di variabili entra nel modello**.

---

## Conclusione

La scelta di un **modello lineare a effetti misti con strategia a blocchi** permette di affrontare il problema in modo trasparente e rigoroso.

L’uso dei blocchi rende possibile:

- separare il contributo dei fattori ambientali, temporali e turistici
- valutare l’effetto specifico del turismo
- costruire un’analisi più facilmente difendibile in un report, una tesi o un paper

L’interesse principale non è soltanto la performance predittiva, ma anche la possibilità di spiegare **quanto e in che modo** le diverse variabili contribuiscono al consumo energetico.
