# Hippo Run - NES Game Makefile
# Uses cc65 toolchain (ca65 assembler, ld65 linker)

# Tools
CA65 = ca65
LD65 = ld65

# Files
ROM = hippo.nes
OBJ = hippo.o
SRC = hippo.asm
CFG = hippo.cfg

# Default target
all: $(ROM)

# Assemble source to object file
$(OBJ): $(SRC)
	$(CA65) -o $(OBJ) $(SRC)

# Link object file to NES ROM
$(ROM): $(OBJ) $(CFG)
	$(LD65) -C $(CFG) -o $(ROM) $(OBJ)

# Clean build files
clean:
	rm -f $(OBJ) $(ROM)

# Rebuild from scratch
rebuild: clean all

# Run in emulator (if fceux is installed)
run: $(ROM)
	@if command -v fceux >/dev/null 2>&1; then \
		fceux $(ROM); \
	elif command -v nestopia >/dev/null 2>&1; then \
		nestopia $(ROM); \
	elif command -v mednafen >/dev/null 2>&1; then \
		mednafen $(ROM); \
	else \
		echo "No NES emulator found. Install fceux, nestopia, or mednafen."; \
		echo "ROM file created: $(ROM)"; \
	fi

.PHONY: all clean rebuild run
