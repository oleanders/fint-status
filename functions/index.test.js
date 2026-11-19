const assert = require("node:assert/strict");
const { test } = require("node:test");
const { proxy } = require("./index");

async function invoke(url, headers = {}) {
    const res = {
        statusCode: 200,
        status(code) { this.statusCode = code; return this; },
        type(value) { this.contentType = value; return this; },
        send(body) { this.body = body; return this; },
        json(body) { this.body = body; return this; },
    };
    await proxy({ url, method: "GET", headers }, res);
    return res;
}

test("forwards FINT requests and headers without following redirects", async (t) => {
    const fetch = t.mock.method(globalThis, "fetch", async () => new Response('{"status":"ok"}'));
    const headers = { authorization: "test-token", "x-org-id": "test-org", "x-client": "test-client" };
    for (const path of ["/https://beta.felleskomponent.no/health", "/proxy/https:/beta.felleskomponent.no/health"]) {
        assert.deepEqual((await invoke(path, headers)).body, { status: "ok" });
    }
    const [url, options] = fetch.mock.calls[0].arguments;
    assert.equal(url.href, "https://beta.felleskomponent.no/health");
    assert.deepEqual(options, { method: "GET", headers: { "Content-Type": "application/json", ...headers }, redirect: "error" });
});

test("preserves plain-text health responses", async (t) => {
    t.mock.method(globalThis, "fetch", async () => new Response("UP"));
    const res = await invoke("/https://beta.felleskomponent.no/health");
    assert.equal(res.body, "UP");
    assert.equal(res.contentType, "text/plain");
});

test("rejects invalid destinations before forwarding credentials", async (t) => {
    const fetch = t.mock.method(globalThis, "fetch", async () => { throw new Error("Must not fetch"); });
    for (const url of ["not a URL", "http://beta.felleskomponent.no", "https://beta.felleskomponent.no.evil.example", "https://beta.felleskomponent.no@evil.example", "https://user:password@beta.felleskomponent.no", "https://beta.felleskomponent.no:8443"]) {
        assert.equal((await invoke("/" + url, { authorization: "test-token" })).statusCode, 400, url);
    }
    assert.equal(fetch.mock.callCount(), 0);
});

test("does not expose upstream error responses", async (t) => {
    t.mock.method(globalThis, "fetch", async () => new Response("private upstream details", { status: 403 }));
    const res = await invoke("/https://beta.felleskomponent.no/health");
    assert.equal(res.statusCode, 500);
    assert.equal(res.body, "Forespørselen til FINT feilet");
});

test("handles network and redirect failures without exposing details", async (t) => {
    t.mock.method(globalThis, "fetch", async () => { throw new Error("private connection details"); });
    const res = await invoke("/https://beta.felleskomponent.no/health");
    assert.equal(res.statusCode, 500);
    assert.equal(res.body, "Forespørselen til FINT feilet");
});
