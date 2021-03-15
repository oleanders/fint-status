# fint-status

Laget dette for å teste Bulma CSS framework Apiene til FINT gjorde det mer inspirerende. I første omgang laget med `play-with-fint`, som har åpne api med CORS-støtte. La til slutt på innlogging til `beta` og `prod`-miljø. Laget da en liten proxy som håndterer forespørsler mot FINT i beta og prod. 

Kjøres lokalt med `npm run dev`

## Bulma CSS framework

* https://bulma.io/documentation/
* https://aramvisser.github.io/bulma-steps/

## Local cors proxy

package.json:
```
 "scripts": {
   "proxy": "lcp --proxyUrl https://beta.felleskomponent.no"
 }
 ```

## API som er i bruk

* Komponentstatus (admin-controller): 
    * Finne helsestatus til komponent: https://play-with-fint.felleskomponent.no/utdanning/elev/admin/health
    * (Caches i en komponent https://play-with-fint.felleskomponent.no/utdanning/elev/admin/caches - denne ikke i bruk)
* API discovery, som finner hver hovedklasse: https://play-with-fint.felleskomponent.no/utdanning/elev/
    * For hver hovedklasse: 
        * Cache size: https://play-with-fint.felleskomponent.no/utdanning/elev/basisgruppe/cache/size
        * Last updated: https://play-with-fint.felleskomponent.no/utdanning/elev/basisgruppe/last-updated


