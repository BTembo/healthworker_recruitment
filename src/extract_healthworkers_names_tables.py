# Get all required packages

import tabula #tabula-py
import os
import re
import pandas as pd
import openpyxl



# This is the folder where all the raw data (.pdf documents) is stored
data_path = r"data/"


# This function is for geting paths for all .pdf files in the folder
def get_path_to_files(FolderPath, FileExt):
	ListOfFilePaths = []
	with os.scandir(FolderPath) as entries:
		for entry in entries:
			print(entry)
			if entry.name.endswith(FileExt):
				file_path = FolderPath + entry.name
				ListOfFilePaths.append(file_path)

	print(ListOfFilePaths)
	return(ListOfFilePaths)


def harvest_data(FolderPath, PathList, PageRange, FileExt):
	ListOfTables = []
	for Path in PathList:
		print(Path)

		FileName = Path
		FileName = re.sub(FolderPath, "", FileName)
		FileName = re.sub(FileExt, "", FileName) # ".pdf"
		FileName = re.sub(" ", "", FileName)
		# FileName = FileName.lower()
		print(FileName)

		ListTable = tabula.read_pdf(Path,
			pages = PageRange,
			stream = True)

		NumberOfTable = len(ListTable)
		print(NumberOfTable)

		for i in range(NumberOfTable):
			DataTable = pd.DataFrame(ListTable[i])
			print(DataTable)

			file_name_csv = "data/output_all/" + FileName + "_tbl_" + str(i) + ".csv"
			# file_name_txt = "data/provincial_pdf/output_all/" + FileName + "_tbl_" + str(i) + ".txt"
			# file_name_xlsx = "data/provincial_pdf/output_all/" + FileName + ".xlsx"
			# xlsx_sheet = "table_" + str(i) 

			DataTable.to_csv(file_name_csv)
			# DataTable.to_csv(file_name_txt)
			# DataTable.to_excel(file_name_xlsx,
			# 	sheet_name = xlsx_sheet) 


def main(FolderPath, PageRange, FileExt):
	files_path_list = get_path_to_files(FolderPath, FileExt)
	harvest_data(FolderPath, files_path_list, PageRange, FileExt)


if __name__ == '__main__':
	main(data_path, 'all', ".pdf")
