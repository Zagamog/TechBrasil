# youtubevideo_transcript.py
import textwrap
import os
from youtube_transcript_api import YouTubeTranscriptApi

# Seminario Juros para Educação
# https://www.youtube.com/live/JeSZGEUUrhM 

# Step 1: Fetch transcript
video_id = "JeSZGEUUrhM"
language_code = "pt"
transcript = YouTubeTranscriptApi.get_transcript(video_id, languages=[language_code])

# Step 2: Format transcript into paragraphs
full_text = " ".join([entry['text'] for entry in transcript])
formatted_text = "\n\n".join(textwrap.wrap(full_text, width=80))  # Wrap at 80 chars

# Step 3: Define LaTeX document structure
latex_template = f"""
\\documentclass[a4paper,12pt]{{article}}
\\usepackage[utf8]{{inputenc}}
\\usepackage[T1]{{fontenc}}
\\usepackage{{geometry}}
\\geometry{{left=2cm, right=2cm, top=2cm, bottom=2cm}}
\\usepackage{{parskip}}

\\title{{YouTube Transcript - {video_id}}}
\\author{{Auto-generated}}
\\date{{}}

\\begin{{document}}

\\maketitle

\\section*{{Transcript}}

{formatted_text}

\\end{{document}}
"""

# Step 4: Define paths
latex_dir = "seminarios/latex/"
tex_filename = os.path.join(latex_dir, "transcript.tex")
pdf_filename = os.path.join(latex_dir, "transcript.pdf")

# Ensure the directory exists
os.makedirs(latex_dir, exist_ok=True)

# Step 5: Save to .tex file
with open(tex_filename, "w", encoding="utf-8") as f:
    f.write(latex_template)

# Step 6: Change working directory to where .tex is located
# NOT os.chdir(latex_dir)

# Step 7: Compile LaTeX to PDF in the correct directory
os.system(f"pdflatex -output-directory={latex_dir} transcript.tex")

print(f"PDF successfully created at: {pdf_filename}")
