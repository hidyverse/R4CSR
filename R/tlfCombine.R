#### Write out combined TLFs
#### heidiesteiner@arizona.edu
#### Jun 15 2023

## option 1

tlf_path <- c(
  "tlf/tlf_base_summarypops.rtf",
  "tlf/intro-ae1.rtf"
)

r2rtf::assemble_rtf(
  input = tlf_path,
  output = "tlf/rtf-combine.rtf"
)

## option 2

r2rtf::assemble_docx(
  tlf_path,
  output = "tlf/rtf-combine-toggle.docx",
  landscape = c(FALSE, FALSE)
)
