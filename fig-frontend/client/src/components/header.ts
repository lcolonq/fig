import { html, css, LitElement } from "lit";
import { customElement } from "lit/decorators.js";

@customElement("fig-header")
export class Header extends LitElement {
  static styles = css`
#header {
  position: absolute;
  text-align: center;
  border-radius: 100px;
  border-style: solid;
  border-color: silver;
  border-width: 5px;
  background: linear-gradient(0deg, rgba(89,89,89,1) 35%, rgba(158,158,158,1) 100%);
  top: 1em;
  left: 33vw;
  width: 33vw;
}
#header:hover {
  filter: brightness(150%);
}
h1 {
  color: #b5a642;
  font-family: "Rubik Maps";
}
`;
  render() {
    return html`
<div id="header">
  <h1>"The JunkYard"</h1>
</div>
`;
  }
}
