import * as Config from "./config";

function generateNonce(): string {
  var arr = new Uint8Array(20);
  window.crypto.getRandomValues(arr);
  return Array.from(arr, b => b.toString(16).padStart(2, "0")).join("");
}

export function startTwitchAuth() {
  const nonce = generateNonce();
  document.cookie = `authnonce=${nonce}; path=/;`;
  window.location.href =
    `https://id.twitch.tv/oauth2/authorize?response_type=id_token`
      + `&client_id=${Config.CLIENT_ID}`
      + `&redirect_uri=${Config.URL}`
      + `&scope=openid`
      + `&nonce=${nonce}`
      + `&claims=${{id_token: {preferred_username: null}}}`
    ;
}

export function getFragmentQuery(): Map<string, string> {
  let query = new Map();
  const hashQuery = document.location.hash.slice(1).split("&");
  for (let equals of hashQuery) {
    const pair = equals.split("=");
    query.set(decodeURIComponent(pair[0]), decodeURIComponent(pair[1]));
  }
  return query;
}

export function getAuthToken(): String | null {
  const frag = getFragmentQuery();
  const token = frag.get("id_token");
  if (token) {
    document.cookie = `id_token=${token}; path=/; SameSite=Strict`;
  }
  for (let c of document.cookie.split("; ")) {
    const [k, v] = c.split("=");
    if (k === "id_token") return v;
  }
  return null;
}
