package main.java.protocol;

public enum MsgType {
    CONN_PUB (TypeGroup.CONTROL),
    DISCONN_PUB (TypeGroup.CONTROL),

    DATA (TypeGroup.DATA);

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
