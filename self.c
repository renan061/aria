
structure Person {
    variable age: Integer;
    initializer {
        self.age = 3;
    }   
}

vira:

structure Person {
    variable age: Integer;
    initializer {
        self = _new(Person)
        _structure_set(self, age, 3)
        return self;
    }
}

---

igualmente:

structure Person {
    value age = 10;
    function getAge: Integer {
        return self.age;
    }
}

vira:

structure Person {
    value age = 10;
    function getAge: Integer {
        return _structure_get(self, age);
    }
}

---

_structure_get e _structure_set são funções que o compilador mapeia para código
LLVM específico.
