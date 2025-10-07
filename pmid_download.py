from Bio import Entrez
import csv

# Provide your email address for identification
Entrez.email = "eideyliu@gmail.com"

# Define the file path for PubMed IDs
# pmid_file_path = "/Users/yangliu/Documents/GitHub/pcv_china/data/literature/pmid-pneumococc-set.txt"
pmid_file_path = "/Users/yangliu/Documents/GitHub/personal_util/citation_MEDLINE50_YJ.txt"

# Read PubMed IDs from file
with open(pmid_file_path, "r") as pmid_file:
    pmid_list = [line.strip() for line in pmid_file.readlines() if line.strip()]  # Collect PubMed IDs

abstracts = []

# Fetch abstracts for each PubMed ID
for pmid in pmid_list:
    fetch_handle = Entrez.efetch(db="pubmed", id=pmid, rettype="abstract", retmode="text")
    abstract = fetch_handle.read().strip()  # Read abstract for current PubMed ID
    fetch_handle.close()
    abstracts.append((pmid, abstract))  # Store (PMID, Abstract) tuple

# Define CSV file path
csv_file = "/Users/yangliu/Downloads/pmid_YJ.csv"

# Write abstracts to CSV
with open(csv_file, "w", newline="", encoding="utf-8") as file:
    writer = csv.writer(file)
    writer.writerow(["PMID", "Abstract"])  # Write header
    writer.writerows(abstracts) 