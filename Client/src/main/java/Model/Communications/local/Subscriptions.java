package Model.Communications.local;

import java.util.*;

//Classe que guarda as subscrições feitas pelo utilizador logado
public class Subscriptions {

    private Map<String, List<SubscriptionObj>> subs;

    public Subscriptions(){
        this.subs = new Hashtable<>();
    }

    private SubscriptionObj parseSub(String subscription){
        String[] tmp = subscription.split("_"); //Braga(1,2) // ConcentrationIncreaseInLocation
        String type = tmp[1];
        String district;
        Integer x ,y;
        if(!type.equals("InfectionsIncrease")){
            String[] tmp2 = subscription.split("\\("); //Braga // 1,2)_ConcentrationIncreaseInLocation
            district = tmp2[0];
            String tmp_pos = tmp2[1]; //1,2)_ConcentrationIncreaseInLocation
            String[] tmp_tmp_pos = tmp_pos.split(","); //1 // 2)_ConcentrationIncreaseInLocation
            String sx = tmp_tmp_pos[0];
            String sy = tmp_tmp_pos[1].split("\\)")[0];
            x = Integer.parseInt(sx);
            y = Integer.parseInt(sy);
            return new SubscriptionObj(district,type,x,y);
        } else {
            String[] tmp2 = subscription.split("_"); //Braga // InfectionsIncrease
            district = tmp2[0];
            return new SubscriptionObj(district,type);
        }
    }

    //Método que recebe subscriçoes na forma (district)(pos)_(type) ex --> Braga(1,2)_ConcentrationIncreaseInLocation e a guarda em um Map
    public void add(String subscription) throws DistrictLimitException {
        SubscriptionObj s = parseSub(subscription);
        this.add(s);
    }

    public void add(SubscriptionObj s) throws DistrictLimitException {
        if(!subs.containsKey(s.getDistrict())){
            if(subs.values().size() >= 3)
                throw new DistrictLimitException("Limit reached");
            else {
                List<SubscriptionObj> l = new ArrayList<>();
                l.add(s);
                subs.put(s.getDistrict(), l);
            }
        }
        else {
            List<SubscriptionObj> l = subs.get(s.getDistrict());
            if(!l.contains(s)) {
                l.add(s);
            }
        }
    }

    //Método que remove a subscrição de um Map
    public void remove(String subscription){
        SubscriptionObj s = parseSub(subscription);
        this.remove(s);
    }

    public void remove(SubscriptionObj s){
        List<SubscriptionObj> l = subs.getOrDefault(s.getDistrict(), new ArrayList<>());
        l.remove(s);
        if(l.size() == 0)
            subs.remove(s.getDistrict());
    }

    public Map<String, List<SubscriptionObj>> getSubs() {
        return subs;
    }
}
