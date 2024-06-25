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

              if (["ContextMenu.clearColumn","edit"].includes(source)) {
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

              // set different values based on the actionType
              // insert_row, remove_row, edit
              console.log(action)
              console.log(hot.toPhysicalRow(action.index))

              switch(action.actionType) {
                case 'change':
                  Shiny.setInputValue(
                    elementId + '_afterundo',
                    {
                      action: action.actionType,
                      row: action.changes.map(x => x[0]).map(x => hot.toPhysicalRow(x)),
                      col: action.changes.map(x => x[1]),
                      // we pick changes[3] for edit in afterChange, but for undo we pick changes[2]
                      val: action.changes.map(x => x[2])
                    }
                  )
                  break;
                case 'insert_row':
                  // its a reverse, so this is data needed for delete of a row
                  Shiny.setInputValue(
                    elementId + '_afterundo',
                      {
                        action: action.actionType,
                        index: hot.toPhysicalRow(action.index),
                        amount: action.amount
                      }
                  )
                  break;
                case 'remove_row':
                  // its a reverse, so this is data needed for insert of a row
                  Shiny.setInputValue(
                  elementId + '_afterundo',
                    {
                      action: action.actionType,
                      index: action.index,
                      sequence: action.rowIndexesSequence,
                      data: action.data,
                      amount: action.amount
                    }
                  )
                  break;
              }
            }
          );

          hot.addHook(
            'afterRedo',
            function(action) {

              console.log('afterredo action', action)

              Shiny.setInputValue(
                  elementId + '_afterredo',
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

