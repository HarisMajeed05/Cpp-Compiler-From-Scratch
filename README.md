# Cpp-Compiler-From-Scratch

A toy C++ compiler implemented in C++ — built from scratch for learning purposes.  
The aim is to explore compiler design concepts such as **lexical analysis**, **parsing**, simple **semantic checks**, and (eventually) **code generation**.

---

## 📂 Project Structure

```text
Cpp-Compiler-From-Scratch/
├── .gitignore
├── Old_Compiler.cpp                          # Core compiler/lexer work-in-progress
├── Updated_Compiler.cpp      # Alternate lexer: with vs. without <regex>
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
g++ -std=c++17 -O2 -o compiler Updated_Compiler.cpp
```

**Windows (MinGW):**

```bash
g++ -std=c++17 -O2 -o compiler.exe Updated_Compiler.cpp
```

### 3) Run

```bash
./compiler input.cpp
# or on Windows
compiler.exe input.cpp
```

---

## 🧪 Example

**Input — `input.cpp`:**

```cpp
int main() {
    return 0;
}
```

**Possible output (tokens/logs depending on the current phase):**

```text
TOK_INT, TOK_IDENTIFIER(main), TOK_LPAREN, TOK_RPAREN, TOK_LBRACE,
TOK_RETURN, TOK_INTLIT(0), TOK_SEMICOLON, TOK_RBRACE
```

---

## 🎯 Learning Goals

- Understand the basic stages of a compiler pipeline
- Implement lexical analysis (with and without `<regex>`)
- Explore simple parsing and error handling
- Prepare the ground for IR and code generation

---

## 🤝 Contributing

Contributions are welcome!

- Fork the repo and create a feature branch: `git checkout -b feat-your-topic`
- Commit with clear messages and open a Pull Request
- Please include small example inputs and expected outputs with changes

---

## 📜 License

This repository is for **educational purposes**.  
License to be decided (**MIT recommended**).
