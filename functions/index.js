const functions = require("firebase-functions");
const rp = require('request-promise');

exports.proxy = functions.https.onRequest((req, res) => {

    // temproary url fix: 
    const requestUrl = "https://" + req.url.replace(/^\/proxy\/https:\//, '');

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
    functions.logger.info("New Request options: ", options);

    rp(options)
        .then(parsedBody => {
            res.send(parsedBody);
        })
        .catch(err => {
            res.status(500).send(err)
        });

});