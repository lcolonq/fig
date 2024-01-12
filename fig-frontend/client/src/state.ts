import { observable, action } from 'mobx';

class Global {
  @observable
  public gizmo_active: boolean = true;

  @action
  public toggle_gizmo() {
    this.gizmo_active = !this.gizmo_active;
  }
}

export const global = new Global();
