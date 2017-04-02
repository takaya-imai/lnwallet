package com.lightning.wallet;


public class Informer {
    public static final int PEER = 1;
    public static final int SYNC = 2;
    public static final int BTCEVENT = 3;
    public static final int CODECHECK = 4;
    public static final int TXCONFIRMED = 5;

    public static final int EMERGENCY = 6;
    public static final int LNSTATE = 7;

    public int tag;
    public String value;
    public Informer(String val, int tag)
    {
        this.value = val;
        this.tag = tag;
    }
}
