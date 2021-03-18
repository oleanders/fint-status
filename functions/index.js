const functions = require("firebase-functions");
const rp = require('request-promise');

exports.proxy = functions.https.onRequest((req, res) => {

    // url fix: 
    const requestUrl = req.url
        .replace(/^\//, '') // fjerner "/"
        .replace(/^proxy\/https:\//, 'https://'); // rydder url i publisert function

    //functions.logger.info("requestUrl: ", requestUrl);

    if (!/^https:\/\/[a-zA-Z-]*\.felleskomponent\.no/.test(requestUrl)) {
        res.status(400).send("Kun for *.felleskomponent.no")
        return
    }

    var options = {
        method: req.method,
        uri: requestUrl,
        body: null,
        headers: {
            'Content-Type': 'application/json',
            'Authorization': req.headers['authorization'],
            'x-org-id': req.headers['x-org-id'],
            'x-client': req.headers['x-client'],
        },
        json: true
    };
    //functions.logger.info("New Request options: ", options);

    rp(options)
        .then(parsedBody => {
            res.send(parsedBody);
        })
        .catch(err => {
            res.status(500).send(err)
        });

});