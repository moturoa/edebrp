with(filter(brp, adres_huishouden == "0aHKcs4Ii_a0U5PhM7Y__"),
     huishouden_functie(
       adres_huishouden,
       anr,
       n_personen_adres,
       institutioneel_adres,
       anrouder1,
       anrouder2,
       anr_partner,
       ouder,
       kind,
       getrouwd_zelfde_adres,
       broerzus,
       leeftijd,
       minder18,
       minder23,
       datum_adres,
       verbose = TRUE
     ))

