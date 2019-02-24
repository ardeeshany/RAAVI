document.addEventListener("DOMContentLoaded", function() {

  var
    example = document.getElementById('example1'),
    hot;

  var mybutton = '<button id="my-btn">Click</button>';


  hot = new Handsontable(example, {
    data: Handsontable.helper.createSpreadsheetData(10, 10),
    rowHeaders: true,
    colWidths: 150,
    columnSorting: true,
    colHeaders: true,
    afterGetRowHeader: function(col, TH) {
     TH.className = 'right'
    }
  });



});