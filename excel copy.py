import xlsxwriter
import xlrd
import xlwt
import math

   def open_file(path):
       wb = xlrd.open_workbook(path)
       sheet = wb.sheet_by_index(0)

       for row_num in range(sheet.nrows):
           row_value = sheet.row_values(row_num)

           if row_value[2] == 68860:
              row = 0
              col = 0

              wbk = xlsxwriter.Workbook('or.xlsx')
              ws = wbk.add_worksheet()

              ws.write('A1', row_value[0])
              ws.write('B1', row_value[1])
              ws.write('C1', row_value[2])
              row = row + 1

              wbk.close()
   if __name__ == "__main__":
       path = "ord.xlsx"
       open_file(path)