require(here)



build_moodle_questions_for_web = function()
{

  if(FALSE)
  {
    assignment_name = "week_02_r_foundations_2"
    assignment_name = "lab_02_r_fundamentals_2"
    assignment_base_dir = "assignments"
    moodle_source_subdir = "moodle"
  }
  
  
  
  get_moodle_question_body = function(filename)
  {
    if (FALSE)
    {
      filename = question_files[1]
    }
    
    file_lines = readLines(filename)
    
    # Read the lines between the markdown header and the end of the questions section
    
    
    
    
  }
  
    
  
  get_rmd_header = function(filename)
  {
    tmp = readLines(filename)
  }
  
  
  
  if (FALSE)
  {
  question_files = get_moodle_quiz_question_files(assignment_name, assignment_base_dir, moodle_source_subdir)  
  
  # get header of first question file
  
  question_files[1]
  
  tmp = readLines(question_files[1])
tmp[1]
  
  
  header_symbols = which(grepl("---", tmp))
  
  out_header = tmp[header_symbols[1]:header_symbols[2]]  
  

  
  
    
    
  }
}

get_moodle_quiz_question_files = function(
      assignment_name,
      assignment_base_dir = "assignments",
      moodle_source_subdir = "moodle"
)
{
  
  if (FALSE)
  {
    
    potential_dirs = list.files(
      path = here::here(assignment_base_dir),
      pattern = assignment_name, 
      recursive = TRUE, 
      include.dirs = TRUE, 
      full.names = TRUE)
    
    
    
    
    # Exclude filename matches
    # potential_dirs = potential_dirs[dir.exists(potential_dirs)]
    assign_dir = potential_dirs[dir.exists(potential_dirs)]
    
    if (length(assign_dir) == 0)
      cat(sprintf("No assignment folder called '%1$s' found...", assignment_name))
    
    if (length(assign_dir) > 1)
      cat(sprintf("Duplicate assignment folders called '%1$s' found...\n Try using a different assignment base directory to limit duplicates", assignment_name))
    
    stopifnot(length(assign_dir) == 1)
    
    cat(sprintf("Assignment folder '%1$s' found at location:\n     '%2$s'", assignment_name, assign_dir))
    
    exercise_dir = file.path(assign_dir, moodle_source_subdir)
    question_files = list.files(path = exercise_dir, pattern = ".Rmd", full.names = TRUE)
    
    q_files = question_files
    
    q_files
    
    if (!is.na(question_numbers))
      q_files = question_files[question_numbers]
    
    q_files
  }
  
  
  
  potential_dirs = list.files(
    path = here::here(assignment_base_dir),
    pattern = assignment_name, 
    recursive = TRUE, 
    include.dirs = TRUE, 
    full.names = TRUE)
  
  
  
  
  # Exclude filename matches
  # potential_dirs = potential_dirs[dir.exists(potential_dirs)]
  assign_dir = potential_dirs[dir.exists(potential_dirs)]
  
  if (length(assign_dir) == 0)
    cat(sprintf("No assignment folder called '%1$s' found...", assignment_name))
  
  if (length(assign_dir) > 1)
    cat(sprintf("Duplicate assignment folders called '%1$s' found...\n Try using a different assignment base directory to limit duplicates", assignment_name))
  
  stopifnot(length(assign_dir) == 1)
  
  cat(sprintf("Assignment folder '%1$s' found at location:\n     '%2$s'", assignment_name, assign_dir))
  
  exercise_dir = file.path(assign_dir, moodle_source_subdir)
  question_files = list.files(path = exercise_dir, pattern = ".Rmd", full.names = TRUE)
  
  return(question_files)
  
}



build_moodle_questions = function(
  assignment_name, 
  assignment_base_dir = "assignments", 
  moodle_source_subdir = "moodle",
  question_numbers = NA, 
  separate_question_files = FALSE)
{
  if(FALSE)
  {
    question_numbers = 1
    separate_question_files = FALSE
    separate_question_files = TRUE
    assignment_name = "week_01_software_setup"
    assignment_base_dir = "assignments"
    moodle_source_subdir = "moodle"
  }
  
  potential_dirs = list.files(
    path = here::here(assignment_base_dir),
    pattern = assignment_name, 
    recursive = TRUE, 
    include.dirs = TRUE, 
    full.names = TRUE)
  
  # potential_dirs = list.dirs(
  # path = here::here(assignment_base_dir), pattern = assignment_name, recursive = TRUE, full.names = TRUE)
  
  # Exclude filename matches
  # potential_dirs = potential_dirs[dir.exists(potential_dirs)]
  assign_dir = potential_dirs[dir.exists(potential_dirs)]
  
  if (length(assign_dir) == 0)
    cat(sprintf("No assignment folder called '%1$s' found...", assignment_name))
  
  if (length(assign_dir) > 1)
    cat(sprintf("Duplicate assignment folders called '%1$s' found...\n Try using a different assignment base directory to limit duplicates", assignment_name))
  
  stopifnot(length(assign_dir) == 1)
  
  cat(sprintf("Assignment folder '%1$s' found at location:\n     '%2$s'", assignment_name, assign_dir))
  
  exercise_dir = file.path(assign_dir, moodle_source_subdir)
  question_files = list.files(path = exercise_dir, pattern = ".Rmd", full.names = TRUE)
  
  q_files = question_files
  if (!is.na(question_numbers))
    q_files = question_files[question_numbers]
  
  # q_files = 
  # ifelse(
  # TRUE,
  # is.na(question_numbers),
  # c(question_files),
  # question_files[question_numbers]
  # )
  
  question_basenames = tools::file_path_sans_ext(basename(q_files))
  
  build_ex = function(f, name = NULL)
  {
    exams::exams2moodle(
      file = f,
      name = name,
      dir = assign_dir,
      edir = exercise_dir,
      iname = FALSE,
      testid = TRUE,
      verbose = TRUE,
      mchoice = list(shuffle = TRUE),
      schoice = list(shuffle = TRUE))
    # schoice = list(shuffle = TRUE),
    # name = assignment_name)
  }
  
  
  if (separate_question_files)
  {
    for (i in 1:length(q_files))
    {
      build_ex(question_files[i], name = question_basenames[i])
    }
  } else {
    build_ex(question_files, name = assignment_name)
  }
  
  
}

build_assignment = function(file_stem, file_prefix = NULL, assignment_dir = here::here("docs", "assignments"))
{
  file_in = paste0(file_stem, ".Rmd")
  assignment_rmd = list.files(path = here::here(), pattern = file_in, recursive = TRUE, full.names = TRUE)
  
  # Make sure the file is found and that there is no duplicate assignment source
  if (length(assignment_rmd) == 0)
    cat(sprintf("Assignment source file '%s' not found.", file_in))
  
  if (length(assignment_rmd) > 1)
  {
    cat(sprintf("Multiple assignment source files with name '%s' found.", file_in))
  }
  
  stopifnot(length(assignment_rmd) == 1)
  
  cat(sprintf("Assignment source file found:%s", assignment_rmd))
  
  file_out = ifelse(
    is.null(file_prefix),
    sprintf("%2$s.html", file_prefix, file_stem),
    sprintf("%1$s_%2$s.html", file_prefix, file_stem)
  )
  
  rmarkdown::render(
    input = assignment_rmd, 
    output_file = file_out,
    output_dir = assignment_dir)
}


build_doc = function(file_stem, dir_out, base_path = here::here(),
                     filename_out = NULL, 
                     type = "html")
{
  render_file = list.files(path = base_path, pattern = paste0(file_stem, ".Rmd"), recursive = TRUE, full.names = TRUE)
  
  if (length(render_file) == 0)
    cat(sprintf("No source file with name '%1$s.%2$s' found.", file_stem, "Rmd"))
  if (length(render_file) > 1)
    cat(sprintf("Duplicate source files with name '%1$s.%2$s' were found.", file_stem, "Rmd"))
  
  stopifnot(length(render_file) == 1)
  
  output_file = 
    sprintf(
      "%1$s.%2$s", 
      paste0(file.path(
        dir_out, 
        ifelse(
          is.null(filename_out),
          file_stem,
          filename_out
        )
      )),
      type)
  
  if (type == "html")
  {
    rmarkdown::render(
      input = render_file, 
      output_file = output_file,
      output_format = "html_document") 
    return(TRUE)
  }
  if (type == "pdf")
  {
    rmarkdown::render(
      input = render_file, 
      output_file = output_file,
      output_format = "pdf_document", 
      output_options = list("toc: TRUE", "number_sections: TRUE"))
    return(TRUE)
  }
  rmarkdown::render(
    input = render_file, 
    output_file = output_file)
  
}
