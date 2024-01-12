import { html, css, LitElement } from "lit";
import { customElement, state } from "lit/decorators.js";

import * as Config from "../config";
import * as State from "../state";
import * as Twitch from "../twitch";

@customElement("fig-footer")
export class Footer extends LitElement {
  private global = State.global;
  static styles = css`
#footer {
  position: absolute;
  bottom: 0px;
  left: 0px;
  right: 0px;
  height: 4ex;
  background: linear-gradient(0deg, rgba(79,39,0,1) 0%, rgba(212,151,113,1) 50%, rgba(213,139,45,1) 100%);
}
button {
  font-family: "Rubik Maps";
  height: 100%;
  color: black;
  border-style: none;
  background: linear-gradient(0deg, rgba(79,39,0,1) 0%, rgba(212,151,113,1) 50%, rgba(213,139,45,1) 100%);
  filter: brightness(125%);
}
button:hover {
  filter: brightness(150%);
}
button:active {
  filter: brightness(75%);
}
`;

  // twitch login
  login() {
    Twitch.startTwitchAuth();
  }
  async check() {
    const resp = await fetch(`${Config.API_URL}/check`);
    console.log(await resp.text());
  }
  button_login() {
    const token = Twitch.getAuthToken();
    console.log(token);
    if (token) {
      return html`
<button @click=${this.check}>CHECK</button>
`;
    } else {
      return html`
<button @click=${this.login}>LOGIN</button>
`;
    }
  }

  // toggle gizmo pane
  toggle_gizmo() {
    console.log(this.global.gizmo_active);
    this.global.toggle_gizmo();
  }

  render() {
    return html`
<div id="footer">
  ${this.button_login()}
  <button @click=${this.toggle_gizmo}>GIZMO</button>
</div>
`;
  }
}
