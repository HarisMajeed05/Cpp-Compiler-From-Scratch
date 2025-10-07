# Cpp-Compiler-From-Scratch

A toy C++ compiler implemented in C++, built from scratch for learning purposes.  
The aim is to explore compiler design concepts such as **lexical analysis**, **parsing** (using Top-to-bottom Recursive descent parsing), simple **semantic checks**, and (eventually) **code generation**.

---

## 📂 Project Structure

```text
Cpp-Compiler-From-Scratch/
├── .gitignore
├── Updated_Compiler.cpp    # Core Lexer                      
├── parser.cpp              # Core parser work-in-progress
└── README.md
```

---

## ⚙️ Getting Started

### 1) Clone the repository

```bash
git clone https://github.com/HarisMajeed05/Cpp-Compiler-From-Scratch.git
cd Cpp-Compiler-From-Scratch
```

### 2) Build

**Linux / macOS (g++ or clang++):**

```bash
g++ -std=c++17 -O2 -o parser parser.cpp
```

**Windows (MinGW):**

```bash
g++ parser.cpp -o parser.exe
```

### 3) Run

```bash
./parser
```

---

## 🎯 Learning Goals

- Understand the basic stages of a compiler pipeline
- Implement lexical analysis (with and without `<regex>`)
- Explore simple parsing and error handling


---

## 📜 License

This repository is for **educational purposes**.  
License to be decided (**MIT recommended**).
