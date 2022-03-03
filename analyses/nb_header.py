# Initialize Stata
from pystata import config
config.init('mp', splash=False)
print(config._get_stata_version_str())

# Define function for displaying HTML table
from IPython.core.display import HTML
from pathlib import Path
def display_table(htmlfile, cssfile=None):
    """Display table created in Stata and exported as HTML
    
    For best results, use the following to export and display the table:
        
        collect style html, useth
        collect export mytable.html, tableonly cssfile() replace
        python: display_table('mytable')
    """
    
    html_path = Path(htmlfile)
    if not html_path.suffix:
        html_path = html_path.with_suffix('.html')
    
    css_path = None
    if cssfile:
        css_path = Path(cssfile)
        if not css_path.suffix:
            css_path = css_path.with_suffix('.css')
    elif html_path.with_suffix('.css').is_file():
        css_path = html_path.with_suffix('.css')
    
    with open(html_path, 'r') as f:
        html = f.read()
    
    extra_css = """table {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
  font-size: 12px;
}
th,td {
  border-width: 1px;
}
"""
    
    if css_path:
        with open(css_path, 'r') as f:
            styles = f'<style>{f.read()}{extra_css}</style>\n\n'
        isolated = True
    else:
        # Without CSS file, fall back on Jupyter CSS
        styles = ''
        isolated = False
    
    display(HTML(styles + html, metadata=dict(isolated=isolated)))

# Change to root of project (modify as necessary)
import os
os.chdir('..')