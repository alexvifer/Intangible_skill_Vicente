# ====================================================================================
# Makefile for Vicente (2026) Heterogeneous Firm Model
#
# DESCRIPTION:
#   Compiles Fortran code for heterogeneous firm dynamics model with intangible
#   capital and financial frictions.
#
# USAGE:
#   make          - Compile the program
#   make clean    - Remove compiled files
#   make run      - Compile and run the program
#
# REQUIREMENTS:
#   - GNU Fortran compiler (gfortran) or Intel Fortran compiler (ifort)
#
# AUTHOR: Generated for Vicente (2026)
# ====================================================================================

# Compiler selection (uncomment one)
FC = gfortran
# FC = ifort

# Compiler flags
# For gfortran:
ifeq ($(FC),gfortran)
    FFLAGS = -O3 -march=native -ffast-math -funroll-loops -fopenmp
    FFLAGS_DEBUG = -O0 -g -Wall -Wextra -fcheck=all -fbacktrace
endif

# For ifort:
ifeq ($(FC),ifort)
    FFLAGS = -O3 -xHost -qopenmp -fast
    FFLAGS_DEBUG = -O0 -g -check all -traceback -warn all
endif

# Directories
SRC_DIR = src
BUILD_DIR = build
OUTPUT_DIR = output

# Source files (in compilation order - modules must come before dependents)
SOURCES = \
    $(SRC_DIR)/mod_parameters.f90 \
    $(SRC_DIR)/mod_globals.f90 \
    $(SRC_DIR)/mod_utility.f90 \
    $(SRC_DIR)/mod_interpolation.f90 \
    $(SRC_DIR)/mod_firm_problem.f90 \
    $(SRC_DIR)/mod_distribution.f90 \
    $(SRC_DIR)/mod_equilibrium.f90 \
    $(SRC_DIR)/main.f90

# Object files
OBJECTS = $(SOURCES:$(SRC_DIR)/%.f90=$(BUILD_DIR)/%.o)

# Module files (generated during compilation)
MODULES = \
    $(BUILD_DIR)/mod_parameters.mod \
    $(BUILD_DIR)/mod_globals.mod \
    $(BUILD_DIR)/mod_utility.mod \
    $(BUILD_DIR)/mod_interpolation.mod \
    $(BUILD_DIR)/mod_firm_problem.mod \
    $(BUILD_DIR)/mod_distribution.mod \
    $(BUILD_DIR)/mod_equilibrium.mod

# Executable name
EXEC = model_vicente

# ====================================================================================
# TARGETS
# ====================================================================================

# Default target
all: directories $(EXEC)

# Create necessary directories
directories:
	@mkdir -p $(BUILD_DIR)
	@mkdir -p $(OUTPUT_DIR)

# Link executable
$(EXEC): $(OBJECTS)
	@echo "Linking $(EXEC)..."
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -o $@ $^
	@echo "Build successful! Executable: $(EXEC)"

# Compile source files to objects
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90
	@echo "Compiling $<..."
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -c $< -o $@

# Debug build
debug: FFLAGS = $(FFLAGS_DEBUG)
debug: clean all

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	rm -rf $(BUILD_DIR)
	rm -f $(EXEC)
	rm -f *.mod
	@echo "Clean complete."

# Clean output files
clean_output:
	@echo "Cleaning output files..."
	rm -f $(OUTPUT_DIR)/*.txt
	@echo "Output cleaned."

# Full clean
distclean: clean clean_output

# Compile and run
run: all
	@echo ""
	@echo "========================================"
	@echo "Running model..."
	@echo "========================================"
	@./$(EXEC)

# Run with timing
time: all
	@echo ""
	@echo "========================================"
	@echo "Running model with timing..."
	@echo "========================================"
	@time ./$(EXEC)

# Help
help:
	@echo "Available targets:"
	@echo "  make          - Compile the program (default)"
	@echo "  make debug    - Compile with debug flags"
	@echo "  make clean    - Remove compiled files"
	@echo "  make run      - Compile and run"
	@echo "  make time     - Compile and run with timing"
	@echo "  make help     - Show this message"
	@echo ""
	@echo "Compiler: $(FC)"
	@echo "Flags: $(FFLAGS)"

# ====================================================================================
# DEPENDENCIES (module hierarchy)
# ====================================================================================

# mod_globals depends on mod_parameters
$(BUILD_DIR)/mod_globals.o: $(BUILD_DIR)/mod_parameters.o

# mod_utility depends on mod_parameters
$(BUILD_DIR)/mod_utility.o: $(BUILD_DIR)/mod_parameters.o

# mod_interpolation depends on mod_parameters and mod_globals
$(BUILD_DIR)/mod_interpolation.o: $(BUILD_DIR)/mod_parameters.o $(BUILD_DIR)/mod_globals.o

# mod_firm_problem depends on mod_parameters, mod_globals, mod_utility, mod_interpolation
$(BUILD_DIR)/mod_firm_problem.o: $(BUILD_DIR)/mod_parameters.o $(BUILD_DIR)/mod_globals.o \
                                  $(BUILD_DIR)/mod_utility.o $(BUILD_DIR)/mod_interpolation.o

# mod_distribution depends on mod_parameters, mod_globals, mod_utility, mod_interpolation
$(BUILD_DIR)/mod_distribution.o: $(BUILD_DIR)/mod_parameters.o $(BUILD_DIR)/mod_globals.o \
                                  $(BUILD_DIR)/mod_utility.o $(BUILD_DIR)/mod_interpolation.o

# mod_equilibrium depends on all previous modules
$(BUILD_DIR)/mod_equilibrium.o: $(BUILD_DIR)/mod_parameters.o $(BUILD_DIR)/mod_globals.o \
                                 $(BUILD_DIR)/mod_utility.o $(BUILD_DIR)/mod_firm_problem.o \
                                 $(BUILD_DIR)/mod_distribution.o

# main depends on all modules
$(BUILD_DIR)/main.o: $(BUILD_DIR)/mod_parameters.o $(BUILD_DIR)/mod_globals.o \
                     $(BUILD_DIR)/mod_utility.o $(BUILD_DIR)/mod_interpolation.o \
                     $(BUILD_DIR)/mod_firm_problem.o $(BUILD_DIR)/mod_distribution.o \
                     $(BUILD_DIR)/mod_equilibrium.o

.PHONY: all directories clean clean_output distclean run time help debug
