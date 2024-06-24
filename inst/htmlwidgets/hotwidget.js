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
            function(changes, source) {

              console.log('afterchange source', source)
              console.log('afterchange changes', changes)

              if (source == 'edit') {
                Shiny.setInputValue(
                  elementId + '_afterchange',
                    {
                      row: changes.map(x => x[0]).map(x => hot.toPhysicalRow(x)),
                      col: changes.map(x => x[1]),
                      val: changes.map(x => x[3])
                    }
                )
              }
            }
          );

          hot.addHook(
            'afterRemoveRow',
            function(index, amount, physicalRows, source) {
              console.log('afterremoverow source', source)

              if (source == 'ContextMenu.removeRow') {
                Shiny.setInputValue(
                  elementId + '_afterremoverow',
                    {
                      index: index,
                      amount: amount,
                      physicalRows: physicalRows
                    }
                  )
              }
            }
          );

          hot.addHook(
            'afterCreateRow',
            function(index, amount, source) {

            console.log('aftercreaterow source', source)
            if (["ContextMenu.rowBelow","ContextMenu.rowAbove"].includes(source)) {
              Shiny.setInputValue(
                elementId + '_aftercreaterow',
                  {
                    index: hot.toPhysicalRow(index),
                    amount: amount
                  }
                )
              }
            }
          );

          hot.addHook(
            'afterRemoveCol',
            function(index, amount, physicalColumns, source) {

              console.log(source)

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
            function(index, amount, source) {

              console.log(source)

              Shiny.setInputValue(
                elementId + '_aftercreatecol',
                {
                  index: index,
                  amount: amount
                }
              )
            }
          );

          hot.addHook(
            'afterUndo',
            function(action) {

              console.log('afterundo action', action)

              Shiny.setInputValue(
                elementId + '_afterundo',
                {
                  action: action
                }
              )

            }
          );

          hot.addHook(
            'afterRedo',
            function(action) {

              console.log('afterredo action', action)

              Shiny.setInputValue(
                elementId + '_afterundo',
                {
                  action: action
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

