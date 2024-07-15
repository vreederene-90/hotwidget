HTMLWidgets.widget({
  name: 'hotwidget',

  type: 'output',

  factory: function(el, width, height) {

    let elementId = el.id;
    const container = document.getElementById(elementId);
    let hot = null;

    return {

      renderValue: function(params) {
        // params contains all options (inc. the data) passed via hotwidget()
        // notably, params.key_column is used to identify the key value in the source data
        //   this is in addition to the natural key that handsontable.js uses, so
        //   you can use this key in order to take a shortcut to update databases
        //   on the R side
        // params.key_column needs to be an integer, because it is incremented by
        //   1 for each new row (do not forget to update your database!)

        // n is used to always trigger the server to update the data
        //  if R sees no changes, the follow up code is not executed
        //  incrementing n fixes that

        let n = 0
        if (hot) {
          hot.updateSettings(params)
        }

        if (!hot) {
          hot = new Handsontable(
            container,
            params
          );

          hot.addHook(
            'beforeChange',
            function(changes, source) {

              for (let i = changes.length - 1; i >= 0; i--) {

                if (params.constraints.unique.includes(changes[i][1])) {

                  let index_col = hot.getColHeader().indexOf(changes[i][1])

                  if (hot.getSourceDataAtCol(index_col).includes(changes[i][3])) {

                    alert("Unique constraint violation: " + changes[i][1] + ": " + changes[i][3])

                    return false;
                  }
                }
              }
            }
          )

          hot.addHook(
            'afterChange',
            function(changes, source) {

              n = n + 1
              let key = null;

              if (params.key_column) key = changes.map(x => x[0]).map(x => hot.getDataAtRowProp(x, params.key_column))

              if (["ContextMenu.clearColumn","edit","Autofill.fill","CopyPaste.cut","CopyPaste.paste"].includes(source)) {
                Shiny.setInputValue(
                  elementId + '_afterchange',
                  {
                    n: n,
                    key: key,
                    key_name: params.key_column,
                    row: changes.map(x => x[0]).map(x => hot.toPhysicalRow(x)),
                    col: changes.map(x => x[1]),
                    val: changes.map(x => x[3])
                  }
                )
              }
            }
          );

          hot.addHook(
            'beforeRemoveRow',
            function(index, amount, physicalRows, source) {

              let index_col = null;
              if (params.key_column) {
                index_col = hot.getColHeader().indexOf(params.key_column)

                if (source == 'ContextMenu.removeRow') {
                  Shiny.setInputValue(
                    elementId + '_beforeremoverow',
                    {
                      n: n,
                      index: index,
                      amount: amount,
                      physicalRows: physicalRows,
                      key: physicalRows.map(x => hot.getDataAtRow(x)[index_col]),
                      key_name: params.key_column
                    }
                  )
                }
              }

            }
          );

          hot.addHook(
            'afterRemoveRow',
            function(index, amount, physicalRows, source) {

              n = n + 1
              // if you want to get the key_column, use beforeremoverow, data is gone here already

              if (source == 'ContextMenu.removeRow') {
                Shiny.setInputValue(
                  elementId + '_afterremoverow',
                  {
                    n: n,
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

              n = n + 1

              let new_index = null;

              if (params.key_column) {
                let new_index_col = hot.getColHeader().indexOf(params.key_column)
                new_index = Math.max(...hot.getSourceDataAtCol(new_index_col)) + 1
                hot.setSourceDataAtCell(hot.toPhysicalRow(index), params.key_column, new_index)
              }

              if (["ContextMenu.rowBelow","ContextMenu.rowAbove"].includes(source)) {
                Shiny.setInputValue(
                  elementId + '_aftercreaterow',
                  {
                    n: n,
                    index: hot.toPhysicalRow(index),
                    key: new_index,
                    key_name: params.key_column,
                    amount: amount
                  }
                )
              }
            }
          );

          // data is lost in afterundo hook, so beforeundo hook is added
          // for case insert_row (actually a delete of a row)
          hot.addHook(
            'beforeUndo',
            function(action) {
              n = n + 1
              switch(action.actionType) {
                case 'insert_row':
                let index_col = null;
                if (params.key_column) index_col = hot.getColHeader().indexOf(params.key_column)
                  Shiny.setInputValue(
                  elementId + '_beforeundo',
                  {
                    n: n,
                    action: action.actionType,
                    key: hot.getSourceDataAtCell(hot.toPhysicalRow(action.index),index_col),
                    key_name: params.key_column,
                    amount: action.amount
                  }
                )
                break;
              }
            }
          );

          hot.addHook(
            'afterUndo',
            function(action) {

              // set different values based on the actionType
              // insert_row, remove_row, edit
              n = n + 1

              switch(action.actionType) {
                case 'change':
                // its a reverse, so this is data needed for undo of a change
                let key = null;
                if (params.key_column) key = action.changes.map(x => x[0]).map(x => hot.getDataAtRowProp(x, params.key_column))

                Shiny.setInputValue(
                  elementId + '_afterundo',
                  {
                    n: n,
                    action: action.actionType,
                    key: key,
                    key_name: params.key_column,
                    row: action.changes.map(x => x[0]).map(x => hot.toPhysicalRow(x)),
                    col: action.changes.map(x => x[1]).map(x => hot.getColHeader(x)),
                    // we pick changes[3] for edit in afterChange, but for undo we pick changes[2]
                    val: action.changes.map(x => x[2])
                  }
                )
                break;
                case 'insert_row':
                // its a reverse, so this is data needed for delete of a row
                // check beforeundo for changes including the key extracted via params.key_column
                Shiny.setInputValue(
                  elementId + '_afterundo',
                  {
                    n: n,
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
                    n: n,
                    action: action.actionType,
                    key_name: params.key_column,
                    index: hot.toPhysicalRow(action.index),
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

              n = n + 1

              switch(action.actionType) {
                case 'change':
                let key = null;
                if (params.key_column) key = action.changes.map(x => x[0]).map(x => hot.getDataAtRowProp(x, params.key_column))

                Shiny.setInputValue(
                  elementId + '_afterredo',
                  {
                    n: n,
                    action: action.actionType,
                    key: key,
                    key_name: params.key_column,
                    row: action.changes.map(x => x[0]).map(x => hot.toPhysicalRow(x)),
                    col: action.changes.map(x => x[1]).map(x => hot.getColHeader(x)),
                    val: action.changes.map(x => x[3])
                  }
                )
                break;
                case 'insert_row':

                Shiny.setInputValue(
                  elementId + '_afterredo',
                  {
                    n: n,
                    action: action.actionType,
                    key: hot.getDataAtRowProp(action.index, params.key_column),
                    key_name: params.key_column,
                    index: hot.toPhysicalRow(action.index),
                    amount: action.amount
                  }
                )
                break;
                case 'remove_row':
                Shiny.setInputValue(
                  elementId + '_afterredo',
                  {
                    n: n,
                    action: action.actionType,
                    data: action.data,
                    key_name: params.key_column
                  }
                )
                break;
              }
            }
          )
        }
      },

      resize: function(width, height) {
      }
    };
  }
});

