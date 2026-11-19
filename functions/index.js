const functions = require("firebase-functions/v1");

exports.proxy = functions.https.onRequest(async (req, res) => {
    const path = req.url.replace(/^\//, "").replace(/^proxy\/https:\//, "https://");
    let url;
    try {
        url = new URL(path);
    } catch {
        return res.status(400).send("Kun for *.felleskomponent.no");
    }
    if (url.protocol !== "https:" || !/^[a-z-]+\.felleskomponent\.no$/.test(url.hostname)
        || url.port || url.username || url.password) {
        return res.status(400).send("Kun for *.felleskomponent.no");
    }

    const headers = { "Content-Type": "application/json" };
    for (const key of ["authorization", "x-org-id", "x-client"]) {
        if (req.headers[key]) headers[key] = req.headers[key];
    }
    try {
        const upstream = await fetch(url, { method: req.method, headers, redirect: "error" });
        if (!upstream.ok) return res.status(500).send("Forespørselen til FINT feilet");
        const body = await upstream.text();
        // Bevar også tekstsvar fra helsesjekkene.
        try {
            return res.json(JSON.parse(body));
        } catch {
            return res.type("text/plain").send(body);
        }
    } catch {
        return res.status(500).send("Forespørselen til FINT feilet");
    }
});
