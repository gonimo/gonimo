window.reflexDomMDC = {};

// Default adapter with all methods set that can be executed directly in JS.
window.reflexDomMDC.Dialog = function (root, dialogSurface, acceptButton) {
    this.root_ = root;
    this.dialogSurface_ = dialogSurface;
    this.acceptButtion_ = acceptButton; // Defaults to undefined.
    this.focusTrap_ = mdc.dialog.util.createFocusTrapInstance(this.dialogSurface_, this.acceptButton_);
    this.foundation_ = this.getDefaultFoundation();
};

window.reflexDomMDC.Dialog.prototype = {
  open: function() {
      this.foundation_.open();
  },
  close: function() {
      this.foundation_.close();
  },
  destroy: function() {
      this.foundation_.destroy();
  },
  getDefaultFoundation: function() {
    return new mdc.dialog.MDCDialogFoundation({
      addClass: (className) => this.addClass(className), // Defined on the Haskell side
      removeClass: (className) => this.removeClass(className), // Defined on the Haskell side
      addBodyClass: (className) => document.body.classList.add(className),
      removeBodyClass: (className) => document.body.classList.remove(className),
      eventTargetHasClass: (target, className) => target.classList.contains(className),
      registerInteractionHandler: (evt, handler) => this.root_.addEventListener(evt, handler),
      deregisterInteractionHandler: (evt, handler) => this.root_.removeEventListener(evt, handler),
      registerSurfaceInteractionHandler: (evt, handler) => this.dialogSurface_.addEventListener(evt, handler),
      deregisterSurfaceInteractionHandler: (evt, handler) => this.dialogSurface_.removeEventListener(evt, handler),
      registerDocumentKeydownHandler: (handler) => document.addEventListener('keydown', handler),
      deregisterDocumentKeydownHandler: (handler) => document.removeEventListener('keydown', handler),
      registerTransitionEndHandler: (handler) => this.dialogSurface_.addEventListener('transitionend', handler),
      deregisterTransitionEndHandler: (handler) => this.dialogSurface_.removeEventListener('transitionend', handler),
      notifyAccept: () => this.notifyAccept(), // Defined on the Haskell side
      notifyCancel: () => this.notifyCancel(), // Defined on the Haskell side
      trapFocusOnSurface: () => this.focusTrap_.activate(),
      untrapFocusOnSurface: () => this.focusTrap_.deactivate(),
      isDialog: (el) => el === this.dialogSurface_,
    });
  }
};
