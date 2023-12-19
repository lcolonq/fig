import { html, css, LitElement } from "lit";
import { customElement, property } from "lit/decorators.js";

import interact from "interactjs";

import * as Config from "./config";
import * as Twitch from "./twitch";

import "./index.css";

console.log("welcome to \"the junkyard\"");

@customElement("fig-window")
export class Window extends LitElement {
  x: number;
  y: number;

  constructor() {
    super();
    this.x = 0;
    this.y = 0;
    interact(this).draggable({
      listeners: {
        move(event) {
          event.target.x += event.dx
          event.target.y += event.dy
          event.target.style.left = `${event.target.x}px`
          event.target.style.top = `${event.target.y}px`
        },
      }
    });
  }

  render() {
    console.log("render");
    return html`
<slot></slot>
`;
  }
}

@customElement("fig-backdrop")
export class Backdrop extends LitElement {
  static styles = css`
#backdrop {
  z-index: -2;
  position: absolute;
  left: 0px;
  top: 0px;
  width: 100vw;
  height: 100vh;
  background-image: url("blueprint.jpg");
}
#blur {
  z-index: -1;
  backdrop-filter: blur(3px);
  position: absolute;
  left: 0px;
  top: 0px;
  width: 100vw;
  height: 100vh;
  background-size: 100px 100px;
  background-position: -20px -20px;
  background-image:
      linear-gradient(to right, lightgrey 1px, transparent 1px),
      linear-gradient(to bottom, lightgrey 1px, transparent 1px);
}
`;
  render() {
    return html`
<div id="backdrop"></div>
<div id="blur"></div>
`;
  }
}

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

@customElement("fig-header")
export class Header extends LitElement {
  static styles = css`
h1 {
  color: #b5a642;
  font-family: "Rubik Maps";
}
`;
  render() {
    return html`
<h1>Welcome To "The JunkYard"</h1>
`;
  }
}
