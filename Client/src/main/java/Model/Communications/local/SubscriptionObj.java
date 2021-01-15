package Model.Communications.local;


import java.util.Objects;

//Classe que representa uma subscrição
public class SubscriptionObj {

    private final Integer x;
    private final Integer y;
    private final String district;
    private final String type;

    public SubscriptionObj(String d, String t, int x, int y){
        this.district = d;
        this.type = t;
        this.x = x;
        this.y = y;
    }

    public SubscriptionObj(String d, String t){
        this.district = d;
        this.type = t;
        this.x = null;
        this.y = null;
    }

    public String getType() {
        return type;
    }

    public String getDistrict() {
        return district;
    }

    public Integer getY() {
        return y;
    }

    public Integer getX() {
        return x;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SubscriptionObj that = (SubscriptionObj) o;
        return Objects.equals(x, that.x) && Objects.equals(y, that.y) && district.equals(that.district) && type.equals(that.type);
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y, district, type);
    }

    @Override
    public String toString() {
        return "Subscription{" +
                "x=" + x +
                ", y=" + y +
                ", district='" + district + '\'' +
                ", type='" + type + '\'' +
                '}';
    }
}
