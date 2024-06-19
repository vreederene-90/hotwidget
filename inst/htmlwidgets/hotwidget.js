HTMLWidgets.widget({

  name: 'hotwidget',

  type: 'output',

  factory: function(el, width, height) {

      return {

        renderValue: function(params) {
          var elementId = el.id;
          const container = document.getElementById(elementId);
          const hot = new Handsontable(container, params)

          hot.addHook(
            'afterChange',
            function(changes) {

              Shiny.setInputValue(
                elementId,
                {
                  data: hot.getSourceData(),
                  headers: hot.getColHeader(),
                  data_types: params.data_types
                }
              )
            }
          )
        },
        resize: function(width, height) {
        }
      };
  }
});

