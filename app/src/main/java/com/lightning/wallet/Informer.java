package com.lightning.wallet;


public class Informer {
    // These should be added on start
    public static final int PEER = 1;
    public static final int LNSTATE = 2;

    // Temporary btc infos
    public static final int BTCEVENT = 3;
    public static final int CHAINSYNC = 4;
    public static final int CODECHECK = 5;
    public static final int TXCONFIRMED = 6;

    // Special emergency state
    public static final int EMERGENCY = 7;

    public int tag;
    public String value;
    public Informer(String val, int tag)
    {
        this.value = val;
        this.tag = tag;
    }
}
