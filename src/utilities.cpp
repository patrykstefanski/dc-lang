#include "utilities.hpp"

#include <cstdio>
#include <optional>
#include <string>

#include "cxx_extensions.hpp"

std::optional<std::string> file_contents(const char* filename) {
  std::FILE* file = std::fopen(filename, "rb");
  if (UNLIKELY(file == nullptr)) {
    return std::nullopt;
  }
  std::string content{};
  std::fseek(file, 0, SEEK_END);
  content.resize(std::ftell(file));
  std::rewind(file);
  // Suppress unused result warnings.
  if (std::fread(&content[0], 1, content.size(), file)) {
  }
  std::fclose(file);
  return content;
}
