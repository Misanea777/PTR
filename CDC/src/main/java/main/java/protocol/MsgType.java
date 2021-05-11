package main.java.protocol;

public enum MsgType {
    conn_pub (TypeGroup.CONTROL),
    disconn_pub (TypeGroup.CONTROL),

    data (TypeGroup.DATA);

    private TypeGroup typeGroup;

    MsgType(TypeGroup typeGroup) {
        this.typeGroup = typeGroup;
    }

    public boolean isInTypeGroup(TypeGroup typeGroup) {
        return this.typeGroup == typeGroup;
    }

    enum TypeGroup {
        CONTROL,
        DATA
    }
}
