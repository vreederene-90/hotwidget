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

              Shiny.setInputValue(
                elementId + '_afterchange',
                {
                  row: changes.map(x => x[0]).map(x => hot.toPhysicalRow(x)),
                  col: changes.map(x => x[1]),
                  val: changes.map(x => x[3])
                }
              )
            }
          );

          hot.addHook(
            'afterRemoveRow',
            function(index, amount, physicalRows) {

              Shiny.setInputValue(
                elementId + '_afterremoverow',
                {
                  index: index,
                  amount: amount,
                  physicalRows: physicalRows
                }
              )
            }
          );

          hot.addHook(
            'afterCreateRow',
            function(index, amount) {

              console.log(hot.getDataAtRow(index+1))
              Shiny.setInputValue(
                elementId + '_aftercreaterow',
                {
                  index: hot.toPhysicalRow(index),
                  amount: amount
                }
              )
            }
          );

          hot.addHook(
            'afterRemoveCol',
            function(index, amount, physicalColumns) {

              Shiny.setInputValue(
                elementId + '_afterremovecol',
                {
                  index: index,
                  amount: amount,
                  physicalColumns: physicalColumns
                }
              )
            }
          );

          hot.addHook(
            'afterCreateCol',
            function(index, amount) {

              Shiny.setInputValue(
                elementId + '_aftercreatecol',
                {
                  index: index,
                  amount: amount
                }
              )
            }
          );
        },
        resize: function(width, height) {
        }
      };
  }
});

