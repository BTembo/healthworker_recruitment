
# Import packages

import codecs
import textract
# import pdfplumber


path =   r"/home/tembo/Documents/modelling/General Analysis/healthworkers_recruitment/data/published names - civil service commission.pdf"

textract_text = textract.process(path)
# print(textract_text)

print(type(textract_text))


# converting
output = textract_text.decode("utf-8")


# with open('/home/tembo/Documents/modelling/General Analysis/healthworkers_recruitment/data/names_health_workers.txt', 'w', encoding = 'utf-8') as f:
# 	f.write(output)
	
with open('/home/tembo/Documents/modelling/General Analysis/healthworkers_recruitment/data/names_health_workers.csv', 'w', encoding = 'utf-8') as f:
	f.write(output)


print(output)
