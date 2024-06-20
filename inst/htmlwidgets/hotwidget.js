HTMLWidgets.widget({

  name: 'hotwidget',

  type: 'output',

  factory: function(el, width, height) {

      return {

        renderValue: function(params) {
          var elementId = el.id;
          const container = document.getElementById(elementId);
          const hot = new Handsontable(container, params);

          hot.addHook(
            'afterChange',
            function(changes) {

              console.log('visual index:', changes.map(x => x[0]))

              // changes.map(x => x[0]).map(x => hot.getData()[x])
              console.log(
                'keys', changes.map(x => x[0]).map(x => hot.getData()[x]).map(x => x[0])
                )

              Shiny.setInputValue(
                elementId,
                {
                  changes: changes,
                  physical_keys: changes.map(x => x[0]).map(x => hot.getData()[x]).map(x => x[0])
                }
              )
            }
          );

          hot.addHook(
            'afterRemoveRow',
            function(index, amount, physicalRows) {

              console.log(index)
              console.log(amount)
              console.log(physicalRows)

            }
          );

          hot.addHook(
            'afterRemoveCol',
            function(index, amount, physicalColumns) {

              console.log(index)
              console.log(amount)
              console.log(physicalColumns)

            }
          );

          hot.addHook(
            'afterCreateRow',
            function(index, amount) {

              console.log(index)
              console.log(amount)

            }
          );

          hot.addHook(
            'afterCreateCol',
            function(index, amount) {

              console.log(index)
              console.log(amount)

            }
          );


        },
        resize: function(width, height) {
        }
      };
  }
});

