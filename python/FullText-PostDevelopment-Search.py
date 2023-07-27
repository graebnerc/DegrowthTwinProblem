#%%
import PyPDF2
import os
import sys
import timeit
import re
import pandas as pd

def test_path():
    if os.getcwd()[-6:] == "python":
        os.chdir("..")
    sys.path.append("./python")
    print(os.getcwd())

#%%
def count_keywords(file_name, return_data_frame=True):
    file_path = os.path.join("data", "core_pdf", file_name)
    pdf_object = PyPDF2.PdfFileReader(file_path, strict=False)
    object_pages = pdf_object.getNumPages()
    
    result_dict = {
        "postdevelopment": {},
        "postcolonial": {},
        "decolonial": {},
        "bibliography": 0
    }
    after_bib = False
    for i in range(0, object_pages):
        # print(i)
        PageObj = pdf_object.getPage(i)
        pdf_text = PageObj.extract_text().lower()
        
        result_bibliography = re.findall("bibliography", pdf_text)
        result_references = re.findall("references", pdf_text)
        
        if len(result_bibliography) + len(result_references) > 0:
            result_dict["bibliography"]  = str(i+1)
            after_bib = True
        elif after_bib is False:
            result_PD = re.findall("postdevelopment", pdf_text)
            result_PDdash = re.findall("post-development", pdf_text)
            result_decolonial = re.findall("decolonial", pdf_text)
            result_PostCol = re.findall("postcolonial", pdf_text)
            result_PostColdash = re.findall("post-colonial", pdf_text)
            result_dict["postdevelopment"][str(i+1)] = len(result_PD) + len(result_PDdash)
            result_dict["postcolonial"][str(i+1)] = len(result_PostCol) + len(result_PostColdash)
            result_dict["decolonial"][str(i+1)] = len(result_decolonial)
        else:
            pass
        
    final_result = {file_name: {}}
    final_result[file_name]["postdevelopment"] = sum(result_dict["postdevelopment"].values())
    final_result[file_name]["postcolonial"] = sum(result_dict["postcolonial"].values())
    final_result[file_name]["decolonial"] = sum(result_dict["decolonial"].values())
    if return_data_frame is True:
        return_object = pd.DataFrame.from_dict(final_result, orient="index")
    else:
        return_object = final_result
    return(return_object)

#%% main
if __name__ == "__main__":
    start = timeit.default_timer()
    test_path()
    files_available = os.listdir(os.path.join("data", "core_pdf"))
    result_path = os.path.join("data", "tidy", "PostDevelopmentSearch.csv")
    overall_results = pd.concat(
        [count_keywords(files_available[i]) for i in range(len(files_available))])
    overall_results.to_csv(result_path)
    
    runtime = timeit.default_timer() - start    
    print("Finished. Total runtime: {} minutes".format(
        round(runtime/60, 2)))
    print("Results saved to: ", result_path)
    exit(0)

# %%
