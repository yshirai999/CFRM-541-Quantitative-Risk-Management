from win32com.client import Dispatch
from win32com.client import constants
xlApp = Dispatch("Excel.Application")
xlApp.DisplayAlerts = False
xlApp.Visible = False
wb=xlApp.Workbooks.Item("OptionsPivotTables.xlsm")
ws=wb.Sheets("OptionsData")


# Define a lambda function to get Excel worksheet column letter from number
excel_col_name = lambda n: '' if n <= 0 else excel_col_name((n - 1) // 26) + chr((n - 1) % 26 + ord('A'))
excel_col_num = lambda a: 0 if a == '' else 1 + ord(a[-1]) - ord('A') + 26 * excel_col_num(a[:-1]) # This is to convert back to number

# Compute number of positions
r = 5
row = ws.Cells(1,1).value
NumPosition = 0
while row is not None:
    NumPosition += 1
    r += 1
    row = ws.Cells(r,1).value

# Define scenarios
S = list([0.8,0.9,0.95,1,1.05,1.1,1.2])
V = list([-0.05,-0.02,0,0.02,0.05,0.1,0.2])
scenarios = [(s,v) for s in S for v in V] # list comprehension to get all scenarios as combinations of underlying and implied volatility scenarios

# Define header of scenario table and adjust column width
c = excel_col_num("AR")
for (s,v) in scenarios:
    ss = str(s)
    vv = str(v)
    ws.Cells(c,4).value = f"Und {round((s-1)*100)}%, ImpVol {round(100*v)}%"
    ws.Columns(c).ColumnWidth = 19*1.23
    #ws.Columns(c).HorizontalAlignment = constants.xlRight
    c += 1


# Loop through all scenarios and fill up table with value change
for r in range(5,NumPosition):
    c = excel_col_num("AR")
    for (s,v) in scenarios:
        ss = str(s)
        vv = str(v)
        fun = f"=(BSPrice($B{r}*{s},$J{r}+{v}, $K{r}, $C{r}, ($I{r}-$B$1)/365, $G{r}, $H{r}, $D{r}) - BSPrice($B{r},$J{r}, $K{r}, $C{r}, ($I{r}-$B$1)/365, $G{r}, $H{r}, $D{r})) * $F{r}"
        ws.Cells(c,r).value = fun
        #ws[excel_col_name(c)+str(r)].alignment = Alignment(horizontal='center')
        #ws.Cells(c,r).number_format = '0'
        c += 1

xlApp.Application.Quit()