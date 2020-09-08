require(here)

get_moodle_question_body = function(filename)
{
  if (FALSE)
  {
    filename = question_files[1]
  }
  
  file_lines = readLines(filename)
  
  # Read the lines between the markdown header and the end of the questions section
  q_line_1 = "Question"
  q_line_2 = "========"
  soln_line_1 = "Solution"
  
  # Find adjacent lines matching the `exams` package question and solution section delimiters
  question_lines = grepl(q_line_1, file_lines)
  soln_lines = grepl(soln_line_1, file_lines)
  delimiter_lines = grepl(q_line_2, file_lines)
  
  # Look for "Question" and "Solution" strings that immediately preced the delimiter string
  q_line = which(question_lines)[(which(delimiter_lines) - 1) == which(question_lines)]
  soln_line = which(soln_lines)[(which(delimiter_lines) - 1) == which(question_lines)]
  
  if (length(q_line) != 1 | length(soln_line) != 1)
    cat(sprintf(
      "Could not locate the Moodle Question and Solution delimiters in file: %s",
      filename))
  
  return(file_lines[(q_line + 2) : (soln_line - 1)])
}


build_moodle_questions_for_web = function(
  assignment_name, 
  out_filename = NULL,
  assignment_base_dir = "assignments",
  moodle_source_subdir = "moodle")
{
  
  if(FALSE)
  {
    # assignment_name = "week_02_r_foundations_2"
    assignment_name = "lab_02_r_fundamentals_2"
    assignment_base_dir = "assignments"
    
    
    assignment_name = "week_02"
    
    assignment_base_dir = file.path("assignments", "eco_602")
    assignment_name = "week_01_data_camp_intro_to_r"
    
    
    moodle_source_subdir = "moodle"
    file_out = here::here("test.Rmd")
    
    build_moodle_questions_for_web(assignment_name, out_filename = file_out, assignment_base_dir = assignment_base_dir, moodle_source_subdir = moodle_source_subdir)
  }
  
  question_paths = get_moodle_quiz_question_files(
    assignment_name, 
    assignment_base_dir, 
    moodle_source_subdir)
  
  question_files = question_paths$question_files
  question_markdown_header = "# Question %0.2d"
  document_lines = get_rmd_header(question_files[1])
  
  # Use the markdown header from the first question for the entire question set:
  # q1_header = get_rmd_header(question_files[1])
  # document_lines = q1_header
  
  
  # q_body_i = get_moodle_question_body(question_files[i])
  # q_body_i = gsub("r CSS", "r", x = q_body_i)
  
  for (i in 1:length(question_files))
  {
    document_lines = c(
      document_lines,      
      c(
        sprintf(fmt = question_markdown_header, i),
        gsub("r CSS", "r", get_moodle_question_body(question_files[i]))
      )
    )
  }
  
  if (!is.null(out_filename))
  {
    writeLines(document_lines, out_filename)
  }
  
  invisible(document_lines)
  
  # writeLines(c(document_lines), "test.Rmd")
  # 
  # if (FALSE)
  # {
  #   question_files = get_moodle_quiz_question_files(assignment_name, assignment_base_dir, moodle_source_subdir)  
  #   # get header of first question file
  #   question_files[1]
  #   tmp = readLines(question_files[1])
  #   tmp[1]
  #   header_symbols = which(grepl("---", tmp))
  #   out_header = tmp[header_symbols[1]:header_symbols[2]]  
  # }
}

get_moodle_quiz_question_files = function(
  assignment_name,
  assignment_base_dir = "assignments",
  moodle_source_subdir = "moodle"
)
{
  potential_dirs = list.files(
    path = here::here(assignment_base_dir),
    pattern = assignment_name, 
    recursive = TRUE, 
    include.dirs = TRUE, 
    full.names = TRUE)
  
  # Exclude filename matches - we are only interested in matching a directory name
  assign_dir = potential_dirs[dir.exists(potential_dirs)]
  
  if (length(assign_dir) == 0)
    cat(sprintf("No assignment folder called '%1$s' found...", assignment_name))
  
  if (length(assign_dir) > 1)
    cat(sprintf("Duplicate assignment folders called '%1$s' found...\n Try using a different assignment base directory to limit duplicates", assignment_name))
  
  stopifnot(length(assign_dir) == 1)
  
  cat(sprintf("Assignment folder '%1$s' found at location:\n     '%2$s'", assignment_name, assign_dir))
  
  exercise_dir = file.path(assign_dir, moodle_source_subdir)
  question_files = list.files(path = exercise_dir, pattern = ".Rmd", full.names = TRUE)
  
  return(list(question_files = question_files, assignment_dir = assign_dir, exercise_dir = exercise_dir))
}


build_moodle_questions = function(
  assignment_name, 
  assignment_base_dir = "assignments", 
  moodle_source_subdir = "moodle",
  question_numbers = NA, 
  separate_question_files = FALSE)
{
  paths = get_moodle_quiz_question_files(assignment_name, assignment_base_dir, moodle_source_subdir)
  
  question_basenames = tools::file_path_sans_ext(basename(paths$question_files))
  question_filenames = paths$question_files
  
  build_ex = function(f, name = NULL)
  {
    exams::exams2moodle(
      file = f,
      name = name,
      dir = paths$assignment_dir,
      edir = paths$exercise_dir,
      iname = FALSE,
      testid = TRUE,
      verbose = TRUE,
      mchoice = list(shuffle = TRUE),
      schoice = list(shuffle = TRUE))
  }
  
  if (separate_question_files)
  {
    for (i in 1:length(question_filenames))
    {
      build_ex(question_filenames[i], name = question_basenames[i])
    }
  } else {
    build_ex(question_filenames, name = assignment_name)
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
