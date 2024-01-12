import { html, css, LitElement } from "lit";
import { customElement } from "lit/decorators.js";

import * as Config from "../config";
import * as Twitch from "../twitch";

@customElement("fig-login")
export class Login extends LitElement {
  static styles = css`
  `;

  login() {
    Twitch.startTwitchAuth();
  }

  async check() {
    const resp = await fetch(`${Config.API_URL}/check`);
    console.log(await resp.text());
  }

  render() {
    const token = Twitch.getAuthToken();
    console.log(token);
    if (token) {
      return html`
<button @click=${this.check}>check token</button>
`;
    } else {
      return html`
<button @click=${this.login}>login</button>
`;
    }
  }
}
