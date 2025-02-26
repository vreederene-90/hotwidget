HTMLWidgets.widget({
  name: "hotwidget",

  type: "output",

  factory: function (el, width, height) {
    let elementId = el.id;
    const container = document.getElementById(elementId);
    let hot = null;
    let params_updated = null;

    return {
      renderValue: function (params) {
        // n is used to always trigger the server to update the data
        //  if R sees no changes, the follow up code is not executed
        //  incrementing n fixes that

        params_updated = params;

        let n = 0;
        if (hot) {
          hot.loadData(params_updated.data);
          hot.updateSettings(params_updated);
        }

        if (!hot) {
          hot = new Handsontable(container, params);

          hot.addHook("beforeChange", function (changes, source) {
            if (params_updated.constraints) {
              for (let i = changes.length - 1; i >= 0; i--) {
                if (params_updated.constraints.unique.includes(changes[i][1])) {
                  let index_col = hot.getColHeader().indexOf(changes[i][1]);

                  let comparable = hot.getSourceDataAtCol(index_col).toString();

                  if (comparable.includes(changes[i][3])) {
                    alert(
                      "Unique constraint violation: " +
                        changes[i][1] +
                        ": " +
                        changes[i][3]
                    );

                    return false;
                  }
                }
              }
            }
          });

          hot.addHook("afterChange", function (changes, source) {
            n = n + 1;
            let key = [];

            if (
              [
                "ContextMenu.clearColumn",
                "edit",
                "Autofill.fill",
                "CopyPaste.cut",
                "CopyPaste.paste",
              ].includes(source)
            ) {
              if (params_updated.key_column) {
                key = changes
                  .map((x) => x[0])
                  .map((y) =>
                    params_updated.key_column.map((z) =>
                      hot.getDataAtRowProp(y, z)
                    )
                  );
              }

              Shiny.setInputValue(elementId + "_afterchange", {
                n: n,
                key: key,
                key_name: params_updated.key_column,
                row: changes.map((x) => x[0]).map((x) => hot.toPhysicalRow(x)),
                col: changes.map((x) => x[1]),
                val: changes.map((x) => x[3]),
              });
            }
          });

          hot.addHook(
            "beforeRemoveRow",
            function (index, amount, physicalRows, source) {
              let key = null;
              if (params_updated.key_column) {
                let visualRows = physicalRows.map((x) => hot.toVisualRow(x));

                key = visualRows.map((x) =>
                  params_updated.key_column.map((y) =>
                    hot.getDataAtRowProp(x, y)
                  )
                );
              }

              if (source == "ContextMenu.removeRow") {
                Shiny.setInputValue(elementId + "_beforeremoverow", {
                  n: n,
                  type: "regular",
                  key: key,
                  key_name: params_updated.key_column,
                });
              }
            }
          );

          hot.addHook("afterCreateRow", function (index, amount, source) {
            n = n + 1;

            let new_index = null;

            if (
              ["ContextMenu.rowBelow", "ContextMenu.rowAbove"].includes(source)
            ) {
              if (params_updated.key_column_plus) {
                // sets value of params_updated.key_column_plus to ++
                // has to be integer, can only be one column
                let new_index_col = hot
                  .getColHeader()
                  .indexOf(params_updated.key_column_plus);
                new_index =
                  Math.max(...hot.getSourceDataAtCol(new_index_col)) + 1;
                hot.setSourceDataAtCell(
                  hot.toPhysicalRow(index),
                  params_updated.key_column_plus,
                  new_index
                );
              }
              Shiny.setInputValue(elementId + "_aftercreaterow", {
                n: n,
                type: "regular",
                index: hot.toPhysicalRow(index),
                key: params_updated.key_column.map((x) =>
                  hot.getDataAtRowProp(index, x)
                ),
                key_plus: params_updated.key_column_plus,
                key_name: params_updated.key_column,
                amount: amount,
              });
            }
          });

          // data is lost in afterundo hook, so beforeundo hook is added
          // for case insert_row (actually a delete of a row)
          hot.addHook("beforeUndo", function (action) {
            n = n + 1;
            switch (action.actionType) {
              case "insert_row":
                let key = null;
                if (params_updated.key_column)
                  key = params_updated.key_column.map((x) =>
                    hot.getDataAtRowProp(action.index, x)
                  );
                Shiny.setInputValue(elementId + "_beforeremoverow", {
                  n: n,
                  type: "undo",
                  key: key,
                  key_name: params_updated.key_column,
                });
                break;
            }
          });

          hot.addHook("afterUndo", function (action) {
            // set different values based on the actionType
            // insert_row, remove_row, edit
            n = n + 1;

            switch (action.actionType) {
              case "change":
                // its a reverse, so this is data needed for undo of a change
                let key = null;
                if (params_updated.key_column)
                  key = action.changes
                    .map((x) => x[0])
                    .map((x) =>
                      params_updated.key_column.map((y) =>
                        hot.getDataAtRowProp(x, y)
                      )
                    );

                Shiny.setInputValue(elementId + "_afterchange", {
                  n: n,
                  key: key,
                  key_name: params_updated.key_column,
                  row: action.changes
                    .map((x) => x[0])
                    .map((x) => hot.toPhysicalRow(x)),
                  col: action.changes
                    .map((x) => x[1])
                    .map((x) => hot.getColHeader(x)),
                  // we pick changes[3] for edit in afterChange, but for undo we pick changes[2]
                  val: action.changes.map((x) => x[2]),
                });
                break;
              case "remove_row":
                // its a reverse, so this is data needed for insert of a row
                Shiny.setInputValue(elementId + "_aftercreaterow", {
                  n: n,
                  type: "undo",
                  key_name: params_updated.key_column,
                  data: action.data,
                });
                break;
            }
          });

          hot.addHook("afterRedo", function (action) {
            n = n + 1;
            let key = null;

            switch (action.actionType) {
              case "change":
                if (params_updated.key_column)
                  key = action.changes
                    .map((x) => x[0])
                    .map((x) =>
                      params_updated.key_column.map((y) =>
                        hot.getDataAtRowProp(x, y)
                      )
                    );

                Shiny.setInputValue(elementId + "_afterchange", {
                  n: n,
                  key: key,
                  key_name: params_updated.key_column,
                  row: action.changes
                    .map((x) => x[0])
                    .map((x) => hot.toPhysicalRow(x)),
                  col: action.changes
                    .map((x) => x[1])
                    .map((x) => hot.getColHeader(x)),
                  val: action.changes.map((x) => x[3]),
                });
                break;
              case "insert_row":
                if (params_updated.key_column_plus) {
                  // sets value of params_updated.key_column_plus to ++
                  // has to be integer, can only be one column
                  let new_index_col = hot
                    .getColHeader()
                    .indexOf(params_updated.key_column_plus);
                  new_index =
                    Math.max(...hot.getSourceDataAtCol(new_index_col)) + 1;
                  hot.setSourceDataAtCell(
                    hot.toPhysicalRow(action.index),
                    params_updated.key_column_plus,
                    new_index
                  );
                }

                Shiny.setInputValue(elementId + "_aftercreaterow", {
                  n: n,
                  type: "redo",
                  key: params_updated.key_column.map((x) =>
                    hot.getDataAtRowProp(action.index, x)
                  ),
                  key_name: params_updated.key_column,
                  index: hot.toPhysicalRow(action.index),
                  amount: action.amount,
                });
                break;
              case "remove_row":
                Shiny.setInputValue(elementId + "_beforeremoverow", {
                  n: n,
                  type: "redo",
                  action: action,
                  data: action.data,
                  key_name: params_updated.key_column,
                });
                break;
            }
          });
        }
      },

      resize: function (width, height) {},
    };
  },
});
