# YADRO

Требуется написать прототип системы, которая следит за работой компьютерного клуба, обрабатывает события и подсчитывает выручку за день и время занятости каждого стола.

# Сборка

```bash
cmake -B build-release -DCMAKE_BUILD_TYPE=Release -S .

cmake --build build-release --target all -j$(nproc)

./build-release/yadro test_file.txt
```
