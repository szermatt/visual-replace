# Configuration file for the Sphinx documentation builder.

# -- Project information

project = 'visual-replace'
copyright = '2020-2024, Stephane Zermatten'
author = 'Stephane Zermatten'

release = '1.1.1snapshot'
version = '1.1.1snapshot'

# -- General configuration

extensions = [
    'sphinx.ext.duration',
    'sphinx.ext.doctest',
    'sphinx.ext.autodoc',
    'sphinx.ext.autosummary',
    'sphinx.ext.intersphinx',
]

root_doc = "index"

intersphinx_mapping = {
    'python': ('https://docs.python.org/3/', None),
    'sphinx': ('https://www.sphinx-doc.org/en/master/', None),
}
intersphinx_disabled_domains = ['std']

templates_path = ['_templates']

# -- Options for HTML output

html_theme = 'sphinx_rtd_theme'

# -- Options for EPUB output
epub_show_urls = 'footnote'

# -- Options for Texinfo output

texinfo_documents = [
    (
        # startdocname
        root_doc,
        # targetname
        "visual-replace",
        # title
        "Visual Replace",
        # author
        "Stephane Zermatten",
        # dir_entry
        "visual-replace",
        # description
        "A nicer interface for string-replace and query-replace that supports previews.",
        # category
        "Emacs",
        # toctree_only
        False,
    )
]
