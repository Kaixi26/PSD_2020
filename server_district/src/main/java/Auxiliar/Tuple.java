package Auxiliar;

public class Tuple<U, V> {
    private U first;
    private V second;

    public Tuple(U first, V second) {
        this.first = first;
        this.second = second;
    }

    public U getFirst() {
        return first;
    }

    public V getSecond() {
        return second;
    }
}
