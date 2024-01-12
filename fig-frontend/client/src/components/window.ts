import { html, css, LitElement } from "lit";
import { customElement, property } from "lit/decorators.js";

import interact from "interactjs";

@customElement("fig-window")
export class Window extends LitElement {
  static z: number = 0;
  
  x: number;
  y: number;

  @property()
  title: string;

  @property()
  hidden: boolean;

  static styles = css`
#windowcontainer {
  z-index: 0;
  position: relative;
  width: 100%;
  height: 100%;
  display: grid;
  grid-template-columns: auto;
  grid-template-rows: 4ex 1fr;
  user-select: none;
}
#title {
  font-family: "Rubik Maps";
  font-size: 150%;
  color: black;
  grid-column: 1;
  grid-row: 1;
  height: 2.2ex;
  overflow: hidden;
  z-index: 2;
  margin-bottom: -1ex;
  background: linear-gradient(0deg, rgba(89,89,89,1) 35%, rgba(158,158,158,1) 100%);
  border-radius: 100px;
  border-style: solid;
  border-color: silver;
  border-width: 5px;
  text-align: center;
}
#window {
  overflow: hidden;
  grid-column: 1;
  grid-row-start: 2;
  grid-row-end: 2;
  z-index: 1;
  background-color: white;
  border-radius: 5ex;
  border-style: solid;
  border-color: black;
  border-width: 2px;
  border-top-width: 0px;
  padding-top: 1ex;
  padding-left: 2ex;
  padding-right: 2ex;
}
`;

  static get(id: string): Window | null {
    const e = document.getElementById(id);
    if (e instanceof Window) return e;
    return null;
  }

  constructor() {
    super();
    this.x = 0;
    this.y = 0;
    this.style.left = `${this.x}px`
    this.style.top = `${this.y}px`
    this.style.width = "300px"
    this.style.height = "300px"
    interact(this).draggable({
      allowFrom: "#title",
      modifiers: [
        interact.modifiers.restrict({
          restriction: "body",
          endOnly: true
        })
      ],
      listeners: {
        move(event) {
          event.target.x += event.dx
          event.target.y += event.dy
          event.target.style.left = `${event.target.x}px`
          event.target.style.top = `${event.target.y}px`
        },
      }
    }).resizable({
      edges: { top: false, bottom: true, left: true, right: true },
      listeners: {
        move(event) {
          event.target.x += event.deltaRect.left
          event.target.y += event.deltaRect.top
          event.target.style.left = `${event.target.x}px`
          event.target.style.top = `${event.target.y}px`
          event.target.style.width = `${Math.max(event.rect.width, 300)}px`
          event.target.style.height = `${Math.max(event.rect.height, 300)}px`
        },
      },
    });
  }

  toggle() {
    this.hidden = !this.hidden;
  }

  click() {
    this.style.zIndex = `${++Window.z}`;
  }

  render() {
    if (!this.hidden) {
      return html`
<div id="windowcontainer" @pointerdown=${this.click}>
  <div id="title">${this.title}</slot></div>
  <div id="window">
    <slot></slot>
  </div>
</div>
`;
    }
  }
}
