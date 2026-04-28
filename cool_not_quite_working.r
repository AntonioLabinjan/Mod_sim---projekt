# ==============================================================================
# PROJEKTNI ZADATAK: MODELIRANJE I SIMULACIJA KASINO IGRE "TRŠĆANSKA BRIŠKULA"
# Kolegij: Modeliranje i simulacije | Programski jezik: R (v4.x)
# ==============================================================================


#' @note Inicijalni model s kaznenim bodovima od -20 pokazao je ekstremnu prednost kuće. 
#' Modifikacijom parametra 'pts' na simetrično bodovanje (+10/-10), model je postao 
#' stabilniji, iako i dalje zadržava negativno očekivanje zbog početne postavke bodova (120).

# --- 1. POSTAVKE SUSTAVA (Globalne varijable) ---
set.seed(42)                         
N_SIMS         <- 10            
INIT_PTS       <- 120                
N_CARDS_DRAWN  <- 10                 
SUITS          <- 4                  
CARDS_PER_SUIT <- 10                 
START_BANKROLL <- 1000               
DPI            <- 300                


print("test")
deck_suits <- rep(1:SUITS, each = CARDS_PER_SUIT) 

# --- 2. LOGIKA MODELA (Računalni model) ---

#' @title Računalna simulacija jedne runde igre
#' @description Izvodi stohastički proces miješanja špila i izvlačenja uzorka karata.
#' @param bet Numerička vrijednost uloga u trenutnoj rundi.
#' @return Lista s rezultatima: broj pogodaka (hits), ostvareni neto profit i faktor isplate.
#' @section Logika: 
#' Koristi diskretnu uniformnu distribuciju za odabir aduta i bez-ponavljajuće 
#' uzorkovanje (sampling without replacement) za simulaciju fizičkog špila.
play_round <- function(bet) {
  # Stohastički odabir boje briškule (aduta)
  briscola_suit <- sample(1:SUITS, 1)          
  # Simulacija miješanja špila (Fisher-Yates shuffle ekvivalent)
  shuffled      <- sample(deck_suits)           
  # Izvlačenje fiksnog uzorka (ruka igrača)
  hand          <- shuffled[1:N_CARDS_DRAWN]    
  
  hits     <- sum(hand == briscola_suit)        
  misses   <- N_CARDS_DRAWN - hits              
  
  # Funkcija cilja: pts = f(hits, misses)
  pts      <- INIT_PTS + hits * 10 - misses * 10 
  mult     <- pts / INIT_PTS                    
  profit   <- (mult - 1) * bet                  
  
  return(list(hits = hits, profit = profit, mult = mult))
}

# --- 3. SIMULACIJA STRATEGIJA (Monte Carlo Engine) ---

#' @title Iterativni Monte Carlo simulator strategija
#' @description Simulira dugoročno kretanje kapitala kroz 10^5 iteracija koristeći različite modalitete klađenja.
#' @param strategy_name Identifikator taktike (Flat, Martingale, Anti-Martingale, Kelly).
#' @param n_sims Ukupan broj runda u simulaciji.
#' @param flat_bet Osnovni ulog za fiksne i progresivne sustave.
#' @return Detaljan vremenski niz bankrolla, uloga i ishoda runda.
simulate_strategy <- function(strategy_name, n_sims = N_SIMS, flat_bet = 10) {
  bankroll    <- numeric(n_sims + 1)
  net_profit  <- numeric(n_sims)
  hits_vec    <- numeric(n_sims)
  bets_vec    <- numeric(n_sims) 
  
  bankroll[1] <- START_BANKROLL
  current_bet <- flat_bet
  
  for (i in seq_len(n_sims)) {
    # Decision Engine: Logika određivanja visine uloga
    bet <- switch(strategy_name,
                  "Flat"           = flat_bet,
                  "Martingale"     = current_bet,
                  "Anti-Martingale"= current_bet,
                  "Kelly"          = max(1, bankroll[i] * 0.05))
    
    # Sigurnosni Constraint: Bankroll ne može pasti ispod nule
    bet <- min(bet, bankroll[i]) 
    bets_vec[i] <- bet           
    
    if (bankroll[i] <= 0) { bankroll[(i+1):(n_sims+1)] <- 0; break }
    
    res <- play_round(bet)
    
    # Ažuriranje stanja sustava
    bankroll[i+1]  <- bankroll[i] + res$profit 
    net_profit[i]  <- res$profit              
    hits_vec[i]    <- res$hits
    
    # Feedback Loop: Prilagodba uloga na temelju prethodnog ishoda
    if (strategy_name == "Martingale") {
      current_bet <- if (res$profit < 0) min(current_bet * 2, 500) else flat_bet
    } else if (strategy_name == "Anti-Martingale") {
      current_bet <- if (res$profit > 0) min(current_bet * 2, 500) else flat_bet
    }
  }
  
  return(list(name = strategy_name, bank = bankroll, prof = net_profit, 
              hits = hits_vec, bets = bets_vec))
}

# --- 4. IZVRŠAVANJE I ANALIZA ---
print("test")
strategies <- c("Flat", "Martingale", "Anti-Martingale", "Kelly")
results    <- lapply(strategies, simulate_strategy)
names(results) <- strategies

strategies
# --- 5. TABLIČNI ISPIS KRETANJA FINANCIJA I STATUS BANKROTA ---

cat("\n==============================================================================\n")
cat("   DETALJAN PREGLED TIJEKA IGRE I ANALIZA PREŽIVLJAVANJA   \n")
cat("==============================================================================\n")

for (strat in strategies) {
  cat(sprintf("\n--- STRATEGIJA: %s ---\n", strat))
  
  # Pronalaženje runde bankrota (prva runda gdje je bankroll <= 0)
  # Ako igrač nije bankrotirao, index će biti NA
  bankruptcy_round <- which(results[[strat]]$bank <= 0)[1] - 1
  
  # Prikaz prvih 10 rundi
  play_log <- data.frame(
    Runda     = 1:10,
    Ulog      = results[[strat]]$bets[1:10],
    Pogoci    = results[[strat]]$hits[1:10],
    Neto_Rez  = round(results[[strat]]$prof[1:10], 2),
    Bankroll  = round(results[[strat]]$bank[2:11], 2)
  )
  
  play_log$Ishod <- ifelse(play_log$Neto_Rez > 0, "DOBITAK", 
                           ifelse(play_log$Neto_Rez < 0, "GUBITAK", "NULA"))
  
  print(play_log, row.names = FALSE)
  
  # ISPIS PORUKE O BANKROTU
  if (!is.na(bankruptcy_round)) {
    cat(sprintf("!!! BANKROT: Igrač je izgubio sav novac u %d. rundi !!!\n", bankruptcy_round))
  } else {
    cat(sprintf("STATUS: Igrač je preživio svih %d runda.\n", N_SIMS))
  }
}
# --- 6. STATISTIČKA VALIDACIJA I VIZUALIZACIJA ---

#' @title Analitička verifikacija modela
#' @description Izračun teorijskog očekivanja pomoću Hipergeometrijske distribucije.
#' Očekivani broj pogodaka E(X) = n * (K/N) = 10 * (10/40) = 2.5.
expected_mult <- (120 + 2.5 * 10 - 7.5 * 10) / 120
rtp_val       <- expected_mult * 100


print(expected_mult)

cat("\n========================================================\n")
cat(sprintf("KONAČNI REZULTAT: Teorijski RTP: %.2f%% | House Edge: %.2f%%\n", rtp_val, 100 - rtp_val))
cat("========================================================\n")

# Generiranje vizualnih izvještaja visoke rezolucije
tiff("01_Analiza_Modela_300dpi.tiff", width=2400, height=1800, res=DPI)
par(mfrow = c(1, 2))
h_counts <- table(factor(results$Flat$hits, levels=0:10))
h_dist   <- h_counts / sum(h_counts)
theory   <- dhyper(0:10, 10, 30, 10)
b <- barplot(h_dist, main="Distribucija aduta (Simulacija vs Teorija)", col="#4C72B0", xlab="Pogoci", ylab="Vjerojatnost")
lines(x = b, y = theory, type="b", pch=18, col="red", lwd=2)
dev.off()

cat("\n✓ Svi izvještaji i tablice su generirani.\n")

cat("test")
# --- 7. ANALIZA EKSTREMNIH DOGAĐAJA ---

#' @title Analiza ekstremnih ishoda: Mitski "10 od 10" scenarij
#' @description 
#' Dok model simulira N_SIMS iteracija, postoji teorijski ishod maksimalnog dobitka 
#' gdje igrač izvlači svih 10 briškula (aduta) iz špila. 
#' 
#' @details
#' Vjerojatnost ovog događaja računa se hipergeometrijskom distribucijom:
#' P(X=10) = (10 choose 10) / (40 choose 10) = 1 / 847,660,528.
#' S obzirom na vjerojatnost od ~1 u 847 milijuna, u simulaciji od 100,000 runda
#' očekivani broj pojavljivanja ovog ishoda je statistički zanemariv (crni labud).

theoretical_p <- 1 / choose(40, 10)
actual_count <- sum(results$Flat$hits == 10)

cat("\n--- ANALIZA EKSTREMNOG DOGAĐAJA (10/10 Briškula) ---\n")
cat(sprintf("Vjerojatnost:          1 u %.0f milijuna\n", (1/theoretical_p) / 1e6))
cat(sprintf("Pojavljivanja u tvojih %.0f simulacija: %d\n", N_SIMS, actual_count))

if(actual_count == 0) {
  cat("Zaključak: Statistički 'crni labud' nije detektiran u uzorku.\n")
}

# Primjer "Best Case" scenarija
ideal_pts     <- 120 + (10 * 10) - (0 * 10) 
ideal_mult    <- ideal_pts / 120            
ideal_profit  <- (ideal_mult - 1) * 1000    

cat(sprintf("\nU slučaju 'Best Case' scenarija (ulog 1000€):\n"))
cat(sprintf("Završni bodovi:        %d\n", ideal_pts))
cat(sprintf("Isplatni faktor:       %.3fx\n", ideal_mult))
cat(sprintf("Neto dobitak:          +%.2f€\n", ideal_profit))

# --- 8. ZBIRNA STATISTIKA (Risk Assessment) ---

cat("\n========================================================\n")
cat("          USPOREDNA STATISTIKA IZDRŽLJIVOSTI           \n")
cat("========================================================\n")

summary_stats <- data.frame(
  Strategija = strategies,
  Preživljeno_Runda = sapply(strategies, function(s) {
    idx <- which(results[[s]]$bank <= 0)[1] - 1
    if(is.na(idx)) N_SIMS else idx
  }),
  Konačni_Bankroll = sapply(strategies, function(s) round(tail(results[[s]]$bank, 1), 2))
)

summary_stats$Status <- ifelse(summary_stats$Preživljeno_Runda < N_SIMS, "BANKROTIRAO", "PREŽIVIO")

print(summary_stats, row.names = FALSE)

# --- 9. OGRANIČENA SIMULACIJA (Limit: 20 Runda) ---

#' @title Simulacija s fiksnim vremenskim horizontom
#' @description Simulira što se događa ako igrač odluči stati nakon točno 20 runda,
#' što je čest scenarij u stvarnom svijetu (ograničeno vrijeme/strpljenje).

N_LIMIT <- 20
cat("\n========================================================\n")
cat(sprintf("   ANALIZA ISHODA NAKON LIMITA OD %d RUNDA   \n", N_LIMIT))
cat("========================================================\n")

# Pokretanje nove simulacije s limitom
results_limit <- lapply(strategies, function(s) simulate_strategy(s, n_sims = N_LIMIT))
names(results_limit) <- strategies

print(strategies)

summary_limit <- data.frame(
  Strategija = strategies,
  Početni_Bankroll = START_BANKROLL,
  Konačni_Bankroll = sapply(strategies, function(s) round(tail(results_limit[[s]]$bank, 1), 2))
)

summary_limit$Neto_Profit <- summary_limit$Konačni_Bankroll - summary_limit$Početni_Bankroll
summary_limit$Ishod <- ifelse(summary_limit$Neto_Profit > 0, "PROFIT", "GUBITAK")

print(summary_limit, row.names = FALSE)

# Vizualizacija kretanja za 20 runda
dev.new() # Otvara novi prozor za grafove ako si u RStudiu
par(mfrow = c(1, 1))
plot(results_limit$Flat$bank, type="l", col="blue", lwd=2, ylim=c(500, 1500),
     main=sprintf("Kretanje kapitala kroz %d runda", N_LIMIT),
     xlab="Runda", ylab="Bankroll (€)")
lines(results_limit$Martingale$bank, col="red", lwd=2)
lines(results_limit$Kelly$bank, col="green", lwd=2)
lines(results_limit$Anti_Martingale$bank, col="orange", lwd=2) # Napomena: provjeri ime u listi
abline(h=START_BANKROLL, lty=2) # Linija nule
legend("topleft", legend=strategies, col=c("blue", "red", "orange", "green"), lwd=2)

cat("\n✓ Analiza limita je gotova. Primijeti razliku u 'preživljavanju'!\n")

# [SVE PRETHODNE SEKCIJE OSTAJU ISTE...]

# --- 10. CASINO OPTIMIZACIJA: ANALIZA LIMITA RUNDI (Ispravljena verzija) ---

stepenice_limita <- seq(10, 100, by = 10)
casino_izvjestaj <- data.frame()

cat("\nPokrećem simulaciju limita...\n")

for (limit in stepenice_limita) {
  
  # Za svaki limit računamo prosjek
  avg_profits <- sapply(strategies, function(s) {
    # 500 replikacija je dovoljno za stabilan prosjek
    runs <- replicate(500, {
      sim <- simulate_strategy(s, n_sims = limit)
      # Profit casina = Početni novac - Završni novac igrača
      START_BANKROLL - tail(sim$bank, 1)
    })
    return(mean(runs))
  })
  
  # PAZI: Ovdje koristimo točne indekse iz avg_profits da izbjegnemo NA
  redak <- data.frame(
    Maks_Rundi = limit,
    Flat       = avg_profits["Flat"],
    Martingale = avg_profits["Martingale"],
    Kelly      = avg_profits["Kelly"],
    Anti_Mart  = avg_profits["Anti-Martingale"]
  )
  
  casino_izvjestaj <- rbind(casino_izvjestaj, redak)
}

# Provjera: ako ima NA, zamijeni ih nulom da plot ne pukne
casino_izvjestaj[is.na(casino_izvjestaj)] <- 0

# Ispis tablice
print(round(casino_izvjestaj, 2), row.names = FALSE)

# Crtanje grafa uz sigurnosnu provjeru ylim
max_y <- max(casino_izvjestaj$Martingale, na.rm = TRUE)
if(is.infinite(max_y) | max_y < 1) max_y <- 100 # Fiksna granica ako podaci ne valjuju

plot(casino_izvjestaj$Maks_Rundi, casino_izvjestaj$Flat, type="b", col="blue", pch=19,
     ylim=c(0, max_y),
     main="Očekivani profit casina po broju dozvoljenih runda",
     xlab="Limit runda", ylab="Prosječna zarada casina (€)")
lines(casino_izvjestaj$Maks_Rundi, casino_izvjestaj$Martingale, type="b", col="red", pch=19)
lines(casino_izvjestaj$Maks_Rundi, casino_izvjestaj$Kelly, type="b", col="green", pch=19)
grid()
legend("topleft", legend=c("Flat", "Martingale", "Kelly"), col=c("blue", "red", "green"), pch=19, lty=1)

cat("\nZAKLJUČAK ZA UPRAVU CASINA:\n")
cat(sprintf("- Ako dozvolite 100 rundi, prosječan igrač na Flat-u će vam ostaviti %.2f€.\n", tail(casino_izvjestaj$Flat, 1)))
cat(sprintf("- Martingale igrači su najisplativiji; prosječno gube %.2f€ u 100 rundi.\n", tail(casino_izvjestaj$Martingale, 1)))

