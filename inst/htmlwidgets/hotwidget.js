HTMLWidgets.widget({

  name: 'hotwidget',

  type: 'output',

  factory: function(el, width, height) {

    var elementId = el.id;
    const container = document.getElementById(elementId);
    let initialized = false;

      return {

        renderValue: function(opts) {
          const hot = new Handsontable(container, opts);
          var that = this;
          if (!initialized) {
            initialized = true;
            container.widget = that;
          }
        },

        resize: function(width, height) {
        }
      };
  }
});
