package com.ojha;


import java.util.HashMap;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;

public class Memo {

    public static void main(String... args) {
        final Memo memo = new Memo();
        memo.g(1);
        memo.g(1);

    }


    private static int MAX = 10;
    private Map<Integer, Integer> lookup = new HashMap<>();
    private Queue<Integer> queue = new PriorityQueue<>();

    public int g(int input) {
        if (lookup.containsKey(input)) {
            return lookup.get(input);
        }

        final int result = this.f(input);
        lookup.put(input, result);
        queue.add(input);
        cacheCheck();

        return result;
    }

    private void cacheCheck() {
        if (lookup.keySet().size() > MAX) {
            final Integer toEvict = queue.remove();
            lookup.remove(toEvict);
        }
    }


    private int f(int input) {
        // expensive
        System.out.println("cat");
        return input;
    }
}
